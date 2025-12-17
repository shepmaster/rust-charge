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

const flavorSchema = z.enum(["daily-for-month"]);

const CALLBACKS = {
  "daily-for-month": {
    tooltip: dateOnlyCategoryTooltip,
    scale: dateOnlyCategoryScale,
  },
};

export default class DivisionUsageForPeriodChartController extends Controller<HTMLCanvasElement> {
  static values = {
    flavor: String,
  };

  declare flavorValue: string;

  async connect() {
    const loader = makeLoader({
      schema: DataSchema,
      dataAttrName: "divisionUsageForPeriodChartValue",
    });

    const data = loader(this.element);

    const flavor = flavorSchema.parse(this.flavorValue);

    const { tooltip, scale } = CALLBACKS[flavor];

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
              title: tooltip,
              label: wattHourTooltip,
            },
          },
        },
        scales: {
          x: scale,
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
