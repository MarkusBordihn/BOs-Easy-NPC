package de.markusbordihn.easynpc.client.screen.editor.action.entry;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.utils.ValueUtils;
import net.minecraft.core.BlockPos;

public class InteractBlockEntry extends ActionEntryWidget {

  private TextField blockPosXTextField;
  private TextField blockPosYTextField;
  private TextField blockPosZTextField;

  public InteractBlockEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasActionData = hasActionData(ActionDataType.INTERACT_BLOCK);
    BlockPos blockPos = hasActionData ? this.actionDataEntry.blockPos() : BlockPos.ZERO;

    // Block Position
    this.blockPosXTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft, editorTop + 20, 70, 16));
    this.blockPosXTextField.setMaxLength(8);
    this.blockPosXTextField.setValue(String.valueOf(blockPos.getX()));
    this.blockPosXTextField.setFilter(ValueUtils::isNumericValue);

    this.blockPosYTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft + 100, editorTop + 20, 70, 16));
    this.blockPosYTextField.setMaxLength(8);
    this.blockPosYTextField.setValue(String.valueOf(blockPos.getY()));
    this.blockPosYTextField.setFilter(ValueUtils::isNumericValue);

    this.blockPosZTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft + 200, editorTop + 20, 70, 16));
    this.blockPosZTextField.setMaxLength(8);
    this.blockPosZTextField.setValue(String.valueOf(blockPos.getZ()));
    this.blockPosZTextField.setFilter(ValueUtils::isNumericValue);
  }

  @Override
  public void render(PoseStack poseStack, int editorLeft, int editorTop) {
    Text.drawString(
        poseStack,
        this.font,
        "Block Position to Interact with:",
        editorLeft + 2,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    return new ActionDataEntry(ActionDataType.INTERACT_BLOCK)
        .withBlockPos(
            new BlockPos(
                Integer.parseInt(this.blockPosXTextField.getValue()),
                Integer.parseInt(this.blockPosYTextField.getValue()),
                Integer.parseInt(this.blockPosZTextField.getValue())));
  }

  @Override
  public boolean hasChanged() {
    boolean hasActionData = hasActionData(ActionDataType.INTERACT_BLOCK);
    BlockPos blockPos = hasActionData ? this.actionDataEntry.blockPos() : BlockPos.ZERO;
    return (this.blockPosXTextField != null
            && !this.blockPosXTextField.getValue().equals(String.valueOf(blockPos.getX()))
        || this.blockPosYTextField != null
            && !this.blockPosYTextField.getValue().equals(String.valueOf(blockPos.getY()))
        || this.blockPosZTextField != null
            && !this.blockPosZTextField.getValue().equals(String.valueOf(blockPos.getZ())));
  }
}
