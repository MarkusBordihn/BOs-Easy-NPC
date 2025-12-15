package de.markusbordihn.easynpc.configui.client.screen.editor.action.entry;

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import net.minecraft.client.gui.GuiGraphics;

public class OpenDefaultDialogEntry extends ActionEntryWidget {

  public OpenDefaultDialogEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    /* Unused */
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.open_default_dialog",
        editorLeft + 2,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    return new ActionDataEntry(ActionDataType.OPEN_DEFAULT_DIALOG);
  }
}
