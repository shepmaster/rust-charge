import { intlFormatDistance } from "date-fns";

export default class RelativeTimestamp extends HTMLElement {
  private now: Date = new Date();
  private update: number | undefined = undefined;

  connectedCallback() {
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
    return this.title || this.textContent;
  }

  render() {
    const originalDate = this.originalDate();
    if (!originalDate) {
      return;
    }

    const then = Date.parse(originalDate);
    const pretty = intlFormatDistance(then, this.now);
    this.textContent = pretty;
    this.title = originalDate;
  }

  disconnectedCallback() {
    if (this.update) {
      window.clearInterval(this.update);
    }
  }
}
