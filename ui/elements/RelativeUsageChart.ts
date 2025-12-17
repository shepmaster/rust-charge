import {
  Chart,
  Legend,
  LineController,
  LineElement,
  LinearScale,
  PointElement,
  TimeScale,
  Tooltip,
} from "chart.js";
import * as z from "zod";
import "chartjs-adapter-date-fns";

import {
  makeLoader,
  absorbNewData,
  missingColorPlugin,
  relativeTimeScale,
  relativeTimeTooltip,
  wattHourTooltip,
  wattHoursScale,
} from "../chartParts";

Chart.register(
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

export default class RelativeUsageChart extends HTMLElement {
  connectedCallback() {
    const canvas = document.createElement("canvas");
    this.appendChild(canvas);

    const loader = makeLoader({
      schema: DataSchema,
      dataAttrName: "relativeUsageChartDataValue",
    });

    const data = loader(this);

    const chart = new Chart(canvas, {
      type: "line",
      data,
      plugins: [missingColorPlugin],
      options: {
        aspectRatio: 16 / 9,
        plugins: {
          legend: {
            position: "bottom",
          },
          tooltip: {
            enabled: true,
            callbacks: {
              title: relativeTimeTooltip,
              label: wattHourTooltip,
            },
          },
        },
        scales: {
          x: relativeTimeScale,
          y: wattHoursScale,
        },
      },
    });

    this.addEventListener(
      "rust-charge:update-inline",
      absorbNewData({ loader, chart }),
    );
  }
}
