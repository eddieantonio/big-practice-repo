"use strict";
function offset() {
    var offsetInMinutes = -new Date().getTimezoneOffset();
    var sign = offsetInMinutes < 0 ? '-' : '+';
    var hours = formatHundreds(offsetInMinutes / 60);
    var minutes = formatHundreds(offsetInMinutes % 60);
    return "" + sign + hours + minutes;
}
exports.offset = offset;
function formatHundreds(num) {
    var magnitude = Math.abs(num);
    var left = ~~(magnitude / 60);
    var right = ~~(magnitude % 60);
    return "" + left + right;
}
