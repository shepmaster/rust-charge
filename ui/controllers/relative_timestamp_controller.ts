import { Controller } from "@hotwired/stimulus";
import { intlFormatDistance } from "date-fns";

export default class RelativeTimestampController extends Controller<HTMLSpanElement> {
  private now: Date = new Date();
  private update: number | undefined = undefined;

  connect() {
    if (!this.originalDate()) {
      return;
    }

    this.render();

    if (!this.update) {
      this.update = window.setInterval(() => {
        this.now = new Date();
        this.render();
      }, 10_000);
    }
  }

  originalDate() {
    return this.element.title || this.element.textContent;
  }

  render() {
    const originalDate = this.originalDate();
    if (!originalDate) {
      return;
    }

    const then = Date.parse(originalDate);
    const pretty = intlFormatDistance(then, this.now);
    this.element.textContent = pretty;
    this.element.title = originalDate;
  }

  disconnect() {
    if (this.update) {
      window.clearInterval(this.update);
    }
  }
}
