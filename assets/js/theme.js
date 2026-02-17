function setTheme(theme) {
	if (theme == 'dark') {
		document.documentElement.setAttribute('data-theme', 'dark');
	} else {
		document.documentElement.removeAttribute('data-theme');
	}
	localStorage.setItem('theme', theme);
}

function getTheme() {
	if (localStorage.getItem('theme') == 'dark') {
		return 'dark';
	}
	return 'light';
}

function initTheme() {
    const saved = localStorage.getItem('theme');
    if (saved) {
        document.documentElement.setAttribute('data-theme', saved);
    } else if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
        document.documentElement.setAttribute('data-theme', 'dark');
    }
}
