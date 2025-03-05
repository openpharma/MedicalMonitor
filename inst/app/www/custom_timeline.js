// This redraws the timeline widget when selecting/deselecting an item. 
// Needed in comination with custom timevis CSS in this package.
function timelineRedrawCustom (el, x){
  this.timeline.on('select', (event, properties) => {
    this.timeline.redraw();
    });
}