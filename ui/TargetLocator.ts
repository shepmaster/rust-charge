export default class TargetLocator {
  private _data_target_attr: string;
  private element: HTMLElement;

  constructor(name: string, element: HTMLElement) {
    this._data_target_attr = `data-${name}-target`;
    this.element = element;
  }

  get data_target_attr() {
    return this._data_target_attr;
  }

  target(name: string) {
    return this.element.querySelector(`[${this.data_target_attr}='${name}']`);
  }

  targets(name: string) {
    return this.element.querySelectorAll(
      `[${this.data_target_attr}='${name}']`,
    );
  }
}
