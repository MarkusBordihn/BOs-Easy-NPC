package de.markusbordihn.easynpc.client.screen.editor.action.entry;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.editor.action.ActionDataEntryEditorScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;

public class CloseDialogEntry extends ActionEntryWidget {

  public CloseDialogEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    // No additional fields required for this action
  }

  @Override
  public void render(PoseStack poseStack, int editorLeft, int editorTop) {
    Text.drawString(
        poseStack,
        this.font,
        "Close Dialog",
        editorLeft + 2,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    return new ActionDataEntry(ActionDataType.CLOSE_DIALOG);
  }

  @Override
  public boolean hasChanged() {
    return !hasActionData(ActionDataType.CLOSE_DIALOG);
  }
}
