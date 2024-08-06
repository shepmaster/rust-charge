import { Duration } from "date-fns";

export function formatWattHours(wattHours: number) {
  const [v, unit] =
    wattHours >= 1000 ? [wattHours / 1000, "kWh"] : [wattHours, "Wh"];
  return `${v.toFixed(2)} ${unit}`;
}

export function formatSecondsAsHMS(rawSeconds: number) {
  return formatDurationAsHMS(secondsToDuration(rawSeconds));
}

export function formatDurationAsHMS(duration: Duration) {
  const hours = `${duration.hours}`.padStart(2, "0");
  const minutes = `${duration.minutes}`.padStart(2, "0");
  const seconds = `${duration.seconds}`.padStart(2, "0");

  return `${hours}:${minutes}:${seconds}`;
}

export function secondsToDuration(rawSeconds: number): Duration {
  const rawMinutes = Math.floor(rawSeconds / 60);
  const seconds = rawSeconds % 60;
  const hours = Math.floor(rawMinutes / 60);
  const minutes = rawMinutes % 60;

  return { hours, minutes, seconds };
}
