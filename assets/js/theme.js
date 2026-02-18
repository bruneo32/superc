function setTheme(theme) {
	if (theme == 'dark')
		document.documentElement.setAttribute('data-theme', 'dark');
	else
		document.documentElement.removeAttribute('data-theme');
	localStorage.setItem('theme', theme);
}

function getTheme() {
	const saved = localStorage.getItem('theme')
	const isDark = !saved
		? window.matchMedia('(prefers-color-scheme: dark)').matches
		: saved == 'dark'
	if (isDark)
		return 'dark';
	return 'light';
}

function initTheme() {
	if (getTheme() == 'dark')
		document.documentElement.setAttribute('data-theme', 'dark');
	else
		document.documentElement.removeAttribute('data-theme');
}
