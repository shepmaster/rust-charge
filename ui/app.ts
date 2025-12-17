import { Application } from "@hotwired/stimulus";
import * as Turbo from "@hotwired/turbo";

import DetectTimezoneController from "./controllers/detect_timezone_controller";
import DivisionUsageForPeriodChartController from "./controllers/division_usage_for_period_chart_controller";
import DurationController from "./controllers/duration_controller";
import FlashNotificationController from "./controllers/flash_notification_controller";
import InfiniteCarouselController from "./controllers/infinite_carousel_controller";
import RelativeTimestampController from "./controllers/relative_timestamp_controller";
import RelativeUsageChartController from "./controllers/relative_usage_chart_controller";

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
window.Stimulus.register("detect-timezone", DetectTimezoneController);
window.Stimulus.register(
  "division-usage-for-period-chart",
  DivisionUsageForPeriodChartController,
);
window.Stimulus.register("duration", DurationController);
window.Stimulus.register("flash-notification", FlashNotificationController);
window.Stimulus.register("infinite-carousel", InfiniteCarouselController);
window.Stimulus.register("relative-timestamp", RelativeTimestampController);
window.Stimulus.register("relative-usage-chart", RelativeUsageChartController);
