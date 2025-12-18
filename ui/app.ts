import htmx from "htmx.org";
import "htmx-ext-sse";

import DetectTimezone from "./elements/DetectTimezone";
import DivisionUsageForPeriodChart from "./elements/DivisionUsageForPeriodChart";
import Duration from "./elements/Duration";
import FlashNotification from "./elements/FlashNotification";
import InfiniteCarousel from "./elements/InfiniteCarousel";
import RelativeTimestamp from "./elements/RelativeTimestamp";
import RelativeUsageChart from "./elements/RelativeUsageChart";

const SWAP_REPLACE = "rc-replace";
const SWAP_UPDATE_INLINE = "rc-update-inline";

function handleSwapReplace(target: HTMLElement, newElement: Element) {
  target.replaceWith(newElement);

  return [target];
}

function handleSwapUpdateInline(target: HTMLElement, newElement: Element) {
  const event = new CustomEvent("rust-charge:update-inline", {
    detail: { newElement },
  });

  target.dispatchEvent(event);

  return [target];
}

htmx.defineExtension("rust-charge", {
  isInlineSwap(swapStyle) {
    return swapStyle === SWAP_REPLACE || swapStyle === SWAP_UPDATE_INLINE;
  },

  handleSwap(swapStyle, target, fragment) {
    if (!(target instanceof HTMLElement)) {
      return false;
    }

    if (!(fragment instanceof DocumentFragment)) {
      return false;
    }

    const newElement = fragment.firstElementChild?.firstElementChild;
    if (!newElement) {
      return false;
    }

    if (swapStyle === SWAP_REPLACE) {
      return handleSwapReplace(target, newElement);
    } else if (swapStyle === SWAP_UPDATE_INLINE) {
      return handleSwapUpdateInline(target, newElement);
    } else {
      return false;
    }
  },
});

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
