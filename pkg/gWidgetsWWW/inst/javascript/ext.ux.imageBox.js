Ext.ux.imageBox = Ext.extend(Ext.Component, {
  value: null,
      initComponent:function() {
      Ext.ux.imageBox.superclass.initComponent.call(this);
    },
      onRender:function(ct, position) {
      this.el = document.createElement("img");
      this.el.id = this.getId();
      this.el.src = this.value;
      Ext.ux.imageBox.superclass.onRender.call(this,ct,position);
    },
      getValue: function() {
      return this.value;
    },
      setValue: function(value) {
      this.value = value;
      document.getElementById(this.id).src = value;
    }
  });
Ext.reg("imagebox", Ext.ux.imageBox);
