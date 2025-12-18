import TargetLocator from "../TargetLocator";

export default class InfiniteCarousel extends HTMLElement {
  private currentSlide: HTMLElement | undefined;
  private observer: IntersectionObserver | undefined;

  connectedCallback() {
    const locate = new TargetLocator("infinite-carousel", this);

    const slidesTarget = locate.target("slides");
    if (!slidesTarget) {
      return;
    }

    const slideTargets = locate.targets("slide");

    const prevTarget = locate.target("prev");
    if (!prevTarget) {
      return;
    }

    const nextTarget = locate.target("next");
    if (!nextTarget) {
      return;
    }

    const mutationObserver = new MutationObserver((mutationList) => {
      for (const mutation of mutationList) {
        for (const addedNode of mutation.addedNodes) {
          if (addedNode instanceof HTMLElement) {
            if (addedNode.getAttribute(locate.data_target_attr) === "slide") {
              this.slideTargetAdded(addedNode);
            }
          }
        }
      }
    });
    mutationObserver.observe(this, { childList: true, subtree: true });

    this.observer = new IntersectionObserver(this.slideVisible, {
      root: slidesTarget,
      threshold: 0.75,
    });

    for (const slide of slideTargets) {
      this.observer.observe(slide);
    }

    prevTarget.classList.remove("hidden");
    nextTarget.classList.remove("hidden");

    prevTarget.addEventListener("click", () => {
      const prev = this.currentSlide?.previousElementSibling;
      prev?.classList.remove("hidden");
      prev?.scrollIntoView();
    });
    nextTarget.addEventListener("click", () => {
      const next = this.currentSlide?.nextElementSibling;
      next?.classList.remove("hidden");
      next?.scrollIntoView();
    });
  }

  slideTargetAdded = (slide: HTMLElement) => {
    this.observer?.observe(slide);
  };

  slideVisible = (intersections: IntersectionObserverEntry[]) => {
    for (const i of intersections) {
      if (i.isIntersecting && i.target instanceof HTMLElement) {
        this.currentSlide = i.target;
      }
    }
  };
}
