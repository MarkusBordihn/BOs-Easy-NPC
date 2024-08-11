package de.markusbordihn.easynpc.client.screen.editor.action.entry;

import de.markusbordihn.easynpc.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;

public class ActionEntryWidget {

  protected final ActionDataEntry actionDataEntry;
  protected final ActionDataSet actionDataSet;
  protected final ActionDataEntryEditorContainerScreen<?> screen;
  protected final Font font;

  public ActionEntryWidget(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    this.actionDataEntry = actionDataEntry;
    this.actionDataSet = actionDataSet;
    this.screen = screen;
    this.font = screen.getFont();
  }

  protected boolean hasActionData(ActionDataType actionDataType) {
    return this.actionDataEntry != null && this.actionDataEntry.actionDataType() == actionDataType;
  }

  public void init(int editorLeft, int editorTop) {}

  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {}

  public ActionDataEntry getActionDataEntry() {
    return this.actionDataEntry;
  }

  public boolean hasChanged() {
    return false;
  }
}
