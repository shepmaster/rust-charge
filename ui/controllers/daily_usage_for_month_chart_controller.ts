import { Controller } from "@hotwired/stimulus";
import {
  CategoryScale,
  Chart,
  Colors,
  Legend,
  BarController,
  BarElement,
  LinearScale,
  Tooltip,
} from "chart.js";
import * as z from "zod";
import "chartjs-adapter-date-fns";

import {
  absorbNewData,
  makeLoader,
  dateOnlyCategoryScale,
  dateOnlyCategoryTooltip,
  wattHourTooltip,
  wattHoursScale,
} from "../chartParts";

Chart.register(
  CategoryScale,
  Colors,
  Legend,
  BarController,
  BarElement,
  LinearScale,
  Tooltip,
);

const DataSchema = z.object({
  datasets: z
    .object({
      label: z.string(),
      data: z
        .object({
          x: z.string(),
          y: z.number(),
        })
        .array(),
    })
    .array(),
});

export default class DailyUsageForMonthChartController extends Controller<HTMLCanvasElement> {
  static values = {
    data: String,
    href: String,
  };

  declare dataValue: string;
  declare readonly hasDataValue: boolean;

  async connect() {
    const loader = makeLoader({
      schema: DataSchema,
      dataAttrName: "dailyUsageForMonthChartValue",
    });

    const data = loader(this.element);

    const chart = new Chart(this.element, {
      type: "bar",
      data,
      options: {
        aspectRatio: 16 / 9,
        plugins: {
          colors: {
            enabled: true,
          },
          legend: { display: false },
          tooltip: {
            enabled: true,
            callbacks: {
              title: dateOnlyCategoryTooltip,
              label: wattHourTooltip,
            },
          },
        },
        scales: {
          x: dateOnlyCategoryScale,
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
