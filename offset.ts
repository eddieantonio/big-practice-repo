export function offset(): string {
    const offsetInMinutes = -new Date().getTimezoneOffset();
    const sign = offsetInMinutes < 0 ? '-' : '+';
    const hours = formatHundreds(offsetInMinutes / 60);
    const minutes = formatHundreds(offsetInMinutes % 60);

    return `${sign}${hours}${minutes}`;
}

function formatHundreds(num: number): string {
    const magnitude = Math.abs(num);
    const left = ~~(magnitude / 60);
    const right = ~~(magnitude % 60);

    return `${left}${right}`;
}
