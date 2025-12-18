import * as Turbo from "@hotwired/turbo";
import "htmx.org";
import "htmx-ext-sse";

import DetectTimezone from "./elements/DetectTimezone";
import DivisionUsageForPeriodChart from "./elements/DivisionUsageForPeriodChart";
import Duration from "./elements/Duration";
import FlashNotification from "./elements/FlashNotification";
import InfiniteCarousel from "./elements/InfiniteCarousel";
import RelativeTimestamp from "./elements/RelativeTimestamp";
import RelativeUsageChart from "./elements/RelativeUsageChart";

interface TurboEventMap {
  "turbo:before-stream-render": Turbo.TurboBeforeStreamRenderEvent;
}

declare global {
  // https://github.com/hotwired/turbo/pull/800
  // eslint-disable-next-line @typescript-eslint/no-empty-object-type
  interface ElementEventMap extends TurboEventMap {}
}

Turbo.session.drive = false;

Turbo.StreamActions["update-inline"] = function () {
  const template = this.firstChild;
  if (!(template instanceof HTMLTemplateElement)) {
    return;
  }

  const newElement = template.content.firstChild;
  if (!newElement) {
    return;
  }

  const event = new CustomEvent("rust-charge:update-inline", {
    detail: { newElement },
  });

  for (const target of this.targetElements) {
    target.dispatchEvent(event);
  }
};

window.customElements.define("rc-detect-timezone", DetectTimezone);
window.customElements.define(
  "rc-division-usage-for-period-chart",
  DivisionUsageForPeriodChart,
);
window.customElements.define("rc-duration", Duration);
window.customElements.define("rc-flash-notification", FlashNotification);
window.customElements.define("rc-infinite-carousel", InfiniteCarousel);
window.customElements.define("rc-relative-timestamp", RelativeTimestamp);
window.customElements.define("rc-relative-usage-chart", RelativeUsageChart);
