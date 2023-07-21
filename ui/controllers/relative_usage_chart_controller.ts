import { Controller } from "@hotwired/stimulus";
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
import { z } from "zod";
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

export default class RelativeUsageChartController extends Controller<HTMLCanvasElement> {
  static values = {
    data: String,
  };

  declare dataValue: string;
  declare readonly hasDataValue: boolean;

  async connect() {
    const loader = makeLoader({
      schema: DataSchema,
      dataAttrName: "relativeUsageChartDataValue",
    });

    const data = loader(this.element);

    const chart = new Chart(this.element, {
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

    this.element.addEventListener(
      "rust-charge:update-inline",
      absorbNewData({ loader, chart }),
    );
  }
}
