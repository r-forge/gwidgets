Ext.example = function(){ 
  var msgCt; function createBox(t, s){ 
    return ["<div class='msg'>", 
	    "<div class='x-box-tl'><div class='x-box-tr'><div class='x-box-tc'></div></div></div>", 
	    "<div class='x-box-ml'><div class='x-box-mr'><div class='x-box-mc'><h3>", 
	    t, "</h3>", s, "</div></div></div>", 
	    "<div class='x-box-bl'><div class='x-box-br'><div class='x-box-bc'></div></div></div>", 
	    "</div>"].join(""); };
  return { 
  msg : function(title, format, delay){ 
      if(!msgCt){ msgCt = Ext.DomHelper.insertFirst(document.body, {id:"msg-div"}, true); }; 
      msgCt.alignTo(document, "t-t"); 
      var s = String.format.apply(String, Array.prototype.slice.call(arguments, 1)); 
      var m = Ext.DomHelper.append(msgCt, {html:createBox(title, s)}, true); 
      m.slideIn("t").pause(delay).ghost("t", {remove:true}); 
    },  
      init: function(){ 
      var t = Ext.get("exttheme"); 
      if(!t){  return; 
      };
      var theme =  "aero"; 
      if(theme){ 
	t.dom.value = theme; 
	Ext.getBody().addClass("x-",theme); 
      }; 
      t.on("change", function(){ setTimeout(function(){ window.location.reload(); }, 250); }); 
      var lb = Ext.get("lib-bar"); 
      if(lb){ lb.show(); } 
    } 
  }; 
}();
// http://extjs.com/forum/showthread.php?t=65632





