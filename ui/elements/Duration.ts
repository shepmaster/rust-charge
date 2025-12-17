import { formatDuration } from "date-fns";

import { formatDurationAsHMS, secondsToDuration } from "../formatters";

export default class Duration extends HTMLElement {
  connectedCallback() {
    const seconds = this.originalSeconds();

    if (!seconds) {
      return;
    }

    const duration = secondsToDuration(seconds);

    this.textContent = formatDurationAsHMS(duration);
    this.title = formatDuration(duration);
    this.dataset["original"] = `${seconds}`;
  }

  originalSeconds() {
    const s = this.originalSecondsText();
    if (s) {
      return parseInt(s, 10);
    }
  }

  originalSecondsText() {
    return this.dataset["original"] || this.textContent;
  }
}
