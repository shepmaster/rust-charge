import TargetLocator from "../TargetLocator";

export default class DetectTimezone extends HTMLElement {
  connectedCallback() {
    const locate = new TargetLocator("detect-timezone", this);

    const buttonTarget = locate.target("button");
    if (!(buttonTarget instanceof HTMLButtonElement)) {
      return;
    }

    const selectTarget = locate.target("select");
    if (!(selectTarget instanceof HTMLSelectElement)) {
      return;
    }

    buttonTarget.classList.remove("hidden");

    buttonTarget.addEventListener("click", (e) => {
      const { timeZone } = Intl.DateTimeFormat().resolvedOptions();
      selectTarget.value = timeZone;
      e.preventDefault();
    });
  }
}
