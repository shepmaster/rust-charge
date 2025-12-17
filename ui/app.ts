import { Application } from "@hotwired/stimulus";
import * as Turbo from "@hotwired/turbo";

import DetectTimezone from "./elements/DetectTimezone";
import DivisionUsageForPeriodChart from "./elements/DivisionUsageForPeriodChart";
import Duration from "./elements/Duration";
import FlashNotification from "./elements/FlashNotification";
import InfiniteCarouselController from "./controllers/infinite_carousel_controller";
import RelativeTimestamp from "./elements/RelativeTimestamp";
import RelativeUsageChart from "./elements/RelativeUsageChart";

interface TurboEventMap {
  "turbo:before-stream-render": Turbo.TurboBeforeStreamRenderEvent;
}

declare global {
  interface Window {
    Stimulus: Application;
  }

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

window.Stimulus = Application.start();
window.Stimulus.register("infinite-carousel", InfiniteCarouselController);

window.customElements.define("rc-detect-timezone", DetectTimezone);
window.customElements.define(
  "rc-division-usage-for-period-chart",
  DivisionUsageForPeriodChart,
);
window.customElements.define("rc-duration", Duration);
window.customElements.define("rc-flash-notification", FlashNotification);
window.customElements.define("rc-relative-timestamp", RelativeTimestamp);
window.customElements.define("rc-relative-usage-chart", RelativeUsageChart);
