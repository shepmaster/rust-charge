import { Controller } from "@hotwired/stimulus";
import { useTargetMutation } from "stimulus-use";

type SlideTarget = HTMLElement;

export default class InfiniteCarouselController extends Controller {
  static targets = ["slides", "slide", "prev", "next"];

  declare readonly slidesTarget: HTMLElement;
  declare readonly slideTargets: SlideTarget[];

  declare readonly prevTarget: HTMLElement;
  declare readonly nextTarget: HTMLElement;

  private currentSlide: HTMLElement | undefined;
  private observer: IntersectionObserver | undefined;

  connect() {
    useTargetMutation(this, { targets: ["slide"] });

    this.observer = new IntersectionObserver(this.slideVisible, {
      root: this.slidesTarget,
      threshold: 0.75,
    });

    for (const slide of this.slideTargets) {
      this.observer.observe(slide);
    }

    this.prevTarget.classList.remove("hidden");
    this.nextTarget.classList.remove("hidden");

    this.prevTarget.addEventListener("click", () => {
      const prev = this.currentSlide?.previousElementSibling;
      prev?.classList.remove("hidden");
      prev?.scrollIntoView();
    });
    this.nextTarget.addEventListener("click", () => {
      const next = this.currentSlide?.nextElementSibling;
      next?.classList.remove("hidden");
      next?.scrollIntoView();
    });
  }

  slideTargetAdded(slide: SlideTarget) {
    this.observer?.observe(slide);
  }

  slideVisible = (intersections: IntersectionObserverEntry[]) => {
    for (const i of intersections) {
      if (i.isIntersecting && i.target instanceof HTMLElement) {
        this.currentSlide = i.target;
      }
    }
  };
}
