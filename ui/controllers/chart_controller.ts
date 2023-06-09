import { Controller } from "@hotwired/stimulus";
import {
  CategoryScale,
  Chart,
  Legend,
  LineController,
  LineElement,
  LinearScale,
  PointElement,
  TimeScale,
  Tooltip,
  ScaleOptions,
} from "chart.js";
import { z } from "zod";
import "chartjs-adapter-date-fns";

import { formatSecondsAsHMS } from "../formatters";
import { UpdateInlineEvent } from "../event";

Chart.register(
  CategoryScale,
  Legend,
  LineController,
  LineElement,
  LinearScale,
  PointElement,
  TimeScale,
  Tooltip,
);

const DataSchema = z.object({
  datasets: z
    .object({
      label: z.string(),
      data: z
        .object({
          x: z.number(),
          y: z.number(),
        })
        .array(),
    })
    .array(),
});

function formatDuration(rawSeconds: number | string) {
  if (typeof rawSeconds === "string") {
    return rawSeconds;
  } else {
    return formatSecondsAsHMS(rawSeconds);
  }
}

const relativeTimeScale: ScaleOptions<"linear"> = {
  type: "linear",
  ticks: {
    callback: formatDuration,
  },
  title: {
    display: true,
    text: "Time since start of charge",
  },
};

const wattHoursScale: ScaleOptions<"linear"> = {
  title: {
    display: true,
    text: "Watt-hours",
  },
};

const missingColorPlugin = (() => {
  // From https://github.com/chartjs/Chart.js/blob/master/src/plugins/plugin.colors.ts
  const RAW_COLORS = [
    [54, 162, 235], // blue
    [255, 99, 132], // red
    [255, 159, 64], // orange
    [255, 205, 86], // yellow
    [75, 192, 192], // green
    [153, 102, 255], // purple
    [201, 203, 207], // grey
  ];

  const BORDER_COLORS = RAW_COLORS.map(([r, g, b]) => `rgb(${r}, ${g}, ${b})`);
  const BACKGROUND_COLORS = RAW_COLORS.map(
    ([r, g, b]) => `rgba(${r}, ${g}, ${b}, 0.5)`,
  );

  let colorIdx = 0;

  return {
    id: "missingColors",

    beforeLayout: (chart: Chart) => {
      for (const dataset of chart.config.data.datasets) {
        if (dataset.borderColor || dataset.backgroundColor) {
          continue;
        }

        dataset.borderColor = BORDER_COLORS[colorIdx];
        dataset.backgroundColor = BACKGROUND_COLORS[colorIdx];

        colorIdx++;
        if (colorIdx >= BORDER_COLORS.length) {
          colorIdx = 0;
        }
      }
    },
  };
})();

// const enableAnimationAfterInitialRenderPlugin = (() => {
//   let savedConfig: Chart["options"]["animation"];
//   let restored = false;

//   return {
//     id: "enable-animation-after-initial-render",

//     afterInit: (chart: Chart) => {
//       savedConfig = chart.options.animation;
//       chart.options.animation = false;
//       console.log("Saved", savedConfig);
//     },

//     afterUpdate: (chart: Chart) => {
//       if (restored) {
//         return;
//       }

//       delete chart.options.animation;
//       chart.options.animation = savedConfig;
//       restored = true;
//       console.log("Restored", savedConfig);
//     },
//   };
// })();

export default class ChartController extends Controller<HTMLCanvasElement> {
  static values = {
    data: String,
    href: String,
    xAxis: String,
    yAxis: String,
  };

  declare dataValue: string;
  declare readonly hasDataValue: boolean;

  declare hrefValue: string;
  declare readonly hasHrefValue: boolean;

  private chart: Chart<"line"> | undefined;

  async connect() {
    const data = await this.getInitialData(DataSchema);

    this.chart = new Chart(this.element, {
      type: "line",
      data,
      plugins: [
        missingColorPlugin,
        // enableAnimationAfterInitialRenderPlugin,
      ],
      options: {
        plugins: {
          legend: {
            position: "bottom",
          },
          tooltip: {
            enabled: true,
            callbacks: {
              title: (ctx) => ctx.map((c) => formatDuration(c.parsed.x)),
              label: (ctx) => `${ctx.parsed.y} Wh`,
            },
          },
        },
        scales: {
          x: relativeTimeScale,
          y: wattHoursScale,
        },
      },
    });

    this.element.addEventListener(
      "rust-charge:update-inline",
      this.absorbNewData,
    );
  }

  absorbNewData = (e: UpdateInlineEvent) => {
    if (!this.chart) {
      return;
    }

    const { newElement } = e.detail;
    if (!(newElement instanceof HTMLCanvasElement)) {
      return;
    }

    const { chartDataValue } = newElement.dataset;
    if (!chartDataValue) {
      return;
    }
    const newRawData = JSON.parse(chartDataValue);
    const newData = this.getData(DataSchema, newRawData);

    const newDataByLabel = new Map(
      newData.datasets.map((d) => [d.label, d.data]),
    );

    this.chart.data.datasets = this.chart.data.datasets.flatMap((ds) => {
      if (!ds.label) {
        return [];
      }

      const newData = newDataByLabel.get(ds.label);
      newDataByLabel.delete(ds.label);

      if (!newData) {
        // This dataset has been removed
        return [];
      }

      ds.data = newData;
      return [ds];
    });

    for (const [label, data] of newDataByLabel) {
      this.chart.data.datasets.push({ label, data });
    }

    this.chart.update();
  };

  async getInitialData<S extends z.ZodTypeAny>(schema: S): Promise<z.infer<S>> {
    const raw = await this.getRawInitialData();
    return this.getData(schema, raw);
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  getData<S extends z.ZodTypeAny>(schema: S, rawData: any): z.infer<S> {
    const parsed = schema.safeParse(rawData);

    if (parsed.success) {
      return parsed.data;
    } else {
      console.log("Data malformed ", parsed.error);
      return { datasets: [] };
    }
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  async getRawInitialData(): Promise<any> {
    if (this.hasDataValue) {
      return JSON.parse(this.dataValue);
    }

    if (this.hasHrefValue) {
      const url = new URL(this.hrefValue, window.location.href);

      const response = await fetch(url);
      return response.json();
    }

    return {};
  }
}
