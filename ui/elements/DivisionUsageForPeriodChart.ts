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
  monthOnlyCategoryScale,
  monthOnlyCategoryTooltip,
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

const flavorSchema = z.enum(["daily-for-month", "monthly-for-year"]);

const CALLBACKS = {
  "daily-for-month": {
    tooltip: dateOnlyCategoryTooltip,
    scale: dateOnlyCategoryScale,
  },
  "monthly-for-year": {
    tooltip: monthOnlyCategoryTooltip,
    scale: monthOnlyCategoryScale,
  },
};

export default class DivisionUsageForPeriodChart extends HTMLElement {
  connectedCallback() {
    const canvas = document.createElement("canvas");
    this.appendChild(canvas);

    const loader = makeLoader({
      schema: DataSchema,
      dataAttrName: "divisionUsageForPeriodChartValue",
    });

    const data = loader(this);

    const flavorText = this.dataset["divisionUsageForPeriodChartFlavorValue"];
    const flavor = flavorSchema.parse(flavorText);

    const { tooltip, scale } = CALLBACKS[flavor];

    const chart = new Chart(canvas, {
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

    this.addEventListener(
      "rust-charge:update-inline",
      absorbNewData({ loader, chart }),
    );
  }
}
