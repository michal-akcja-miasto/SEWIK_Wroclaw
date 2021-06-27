(function(){
const appHeight = () => {
    const doc = document.documentElement
    doc.style.setProperty('--app-height', `${window.innerHeight/10}rem`)
}
window.addEventListener('resize', appHeight)
window.addEventListener('orientationchange', appHeight)
appHeight()
})()