import { Controller } from "@hotwired/stimulus";
import { formatDuration } from "date-fns";

import { formatDurationAsHMS, secondsToDuration } from "../formatters";

export default class DurationController extends Controller<HTMLSpanElement> {
  connect() {
    const seconds = this.originalSeconds();

    if (!seconds) {
      return;
    }

    const duration = secondsToDuration(seconds);

    this.element.textContent = formatDurationAsHMS(duration);
    this.element.title = formatDuration(duration);
    this.element.dataset["original"] = `${seconds}`;
  }

  originalSeconds() {
    const s = this.originalSecondsText();
    if (s) {
      return parseInt(s, 10);
    }
  }

  originalSecondsText() {
    return this.element.dataset["original"] || this.element.textContent;
  }
}
