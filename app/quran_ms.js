// quran_ms
// Author: Wan Nor Arifin
function doCalculate() {
    khatam = +document.quran_ms.khatam.value;
    khatam_total = +document.quran_ms.khatam_total.value;
    if (khatam_total > khatam) {
        khatam_total = khatam;
        document.quran_ms.khatam_total.value = khatam;
    }
    ms_total = +document.quran_ms.ms_total.value;
    ms_read = +document.quran_ms.ms_read.value;
    if (ms_read > ms_total) {
        ms_read = ms_total;
        document.quran_ms.ms_read.value = ms_total;
    }
    day_pass = +document.quran_ms.day_pass.value;
    day_total = +document.quran_ms.day_total.value;
    if (day_pass > day_total) {
        day_pass = day_total;
        document.quran_ms.day_pass.value = day_total;
    }

    ms_day = (ms_total*khatam - khatam_total*ms_total - ms_read) / (day_total - (day_pass - 1));
    if (ms_day < 0) {
        ms_day = 0;
    }
    ms_solat = ms_day/5;

    document.quran_ms.ms_day.value = ms_day.toFixed(1);
    document.quran_ms.ms_solat.value = ms_solat.toFixed(1);

    return;
}
