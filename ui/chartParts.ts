import { formatDate as dateFnFormatDate, formatISO } from "date-fns";
import {
  Chart,
  ScaleOptions,
  TooltipItem,
  ChartData,
  ChartDataset,
  ChartType,
} from "chart.js";
import { z } from "zod";
import "chartjs-adapter-date-fns";

import { UpdateInlineEvent } from "./event";
import { formatSecondsAsHMS, formatWattHours } from "./formatters";

export const missingColorPlugin = (() => {
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

function formatDuration(rawSeconds: number | string) {
  if (typeof rawSeconds === "string") {
    return rawSeconds;
  } else {
    return formatSecondsAsHMS(rawSeconds);
  }
}

const formatDate = (rawDateString: string): string => {
  const date = new Date(rawDateString);
  return formatISO(date, { representation: "date" });
};

const formatDay = (rawDateString: string): string => {
  const date = new Date(rawDateString);
  return dateFnFormatDate(date, "dd");
};

export const relativeTimeScale: ScaleOptions<"linear"> = {
  type: "linear",
  ticks: {
    callback: formatDuration,
  },
  title: {
    display: true,
    text: "Time since start of charge",
  },
};

export const dateOnlyCategoryScale: ScaleOptions<"category"> = {
  type: "category",
  ticks: {
    callback(tickValue) {
      if (typeof tickValue === "string") {
        return tickValue;
      }
      const originalLabel = this.getLabelForValue(tickValue);
      return formatDay(originalLabel);
    },
  },
  title: {
    display: false,
  },
};

export const wattHoursScale: ScaleOptions<"linear"> = {
  ticks: {
    callback(tickValue) {
      if (typeof tickValue === "string") {
        return tickValue;
      } else {
        return formatWattHours(tickValue);
      }
    },
  },
  title: {
    display: true,
    text: "Energy",
  },
};

type OurTooltipItem = TooltipItem<"line"> | TooltipItem<"bar">;

export const relativeTimeTooltip = <C extends OurTooltipItem>(ctx: C[]) =>
  ctx.map((c) => formatDuration(c.parsed.x));

export const dateOnlyCategoryTooltip = <C extends OurTooltipItem>(ctx: C[]) =>
  ctx.map((c) => formatDate(c.label));

export const wattHourTooltip = <C extends OurTooltipItem>(ctx: C) =>
  formatWattHours(ctx.parsed.y);

interface MakeLoaderProps<S> {
  schema: S;
  dataAttrName: string;
}

export const makeLoader =
  <
    TType extends ChartType,
    TData,
    S extends z.ZodType<ChartData<TType, TData>>,
  >(
    props: MakeLoaderProps<S>,
  ) =>
  (element: HTMLCanvasElement): z.infer<S> => {
    const { dataAttrName, schema } = props;
    const rawStringData = element.dataset[dataAttrName];

    let rawData;
    if (rawStringData) {
      rawData = JSON.parse(rawStringData);
    } else {
      return { datasets: [] };
    }

    const parsed = schema.safeParse(rawData);

    if (parsed.success) {
      return parsed.data;
    } else {
      console.log("Data malformed ", parsed.error);
      return { datasets: [] };
    }
  };

interface AbsorbNewDataProps<
  TType extends ChartType,
  TData,
  S extends z.ZodType<ChartData<TType, TData>>,
> {
  chart: Chart<TType, TData>;
  loader: (element: HTMLCanvasElement) => z.infer<S>;
}

export const absorbNewData =
  <
    TType extends ChartType,
    TData,
    S extends z.ZodType<ChartData<TType, TData>>,
  >(
    props: AbsorbNewDataProps<TType, TData, S>,
  ) =>
  (e: UpdateInlineEvent) => {
    const { chart, loader } = props;

    const { newElement } = e.detail;
    if (!(newElement instanceof HTMLCanvasElement)) {
      return;
    }

    const newData = loader(newElement);

    const newDataByLabel = new Map(
      newData.datasets.map((d) => [d.label, d.data]),
    );

    chart.data.datasets = chart.data.datasets.flatMap((ds) => {
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
      // I'm not smart enough to figure out the TS error here
      const set = { label, data } as ChartDataset<
        ChartType,
        TData
      > as ChartDataset<TType, TData>;
      chart.data.datasets.push(set);
    }

    chart.update();
  };
