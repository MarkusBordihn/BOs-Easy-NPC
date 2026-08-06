/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.configui.client.screen.editor.action.entry;

import de.markusbordihn.easynpc.api.pose.ModelPoseAPI;
import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import java.util.LinkedHashSet;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.resources.Identifier;

public class PoseActionEntry extends ActionEntryWidget {

  private static final String CUSTOM_POSE_ID = "custom";
  private static final int ROW_WIDTH = 285;

  private SpinButton<String> poseSelectionButton;
  private TextField poseIdField;

  public PoseActionEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    LinkedHashSet<String> poseSelection = new LinkedHashSet<>();
    ModelPoseAPI.getVanillaPoseIds().forEach(poseId -> poseSelection.add(poseId.toString()));
    ModelPoseAPI.getAvailablePoses(this.screen.getEasyNPC())
        .forEach(poseId -> poseSelection.add(poseId.toString()));
    poseSelection.add(CUSTOM_POSE_ID);

    String storedPoseId =
        hasActionData(ActionDataType.SET_POSE) ? this.actionDataEntry.poseId().trim() : "";
    String selectedPoseId;
    if (storedPoseId.isEmpty()) {
      selectedPoseId = poseSelection.iterator().next();
    } else if (poseSelection.contains(storedPoseId)) {
      selectedPoseId = storedPoseId;
    } else {
      selectedPoseId = CUSTOM_POSE_ID;
    }

    this.poseIdField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft, editorTop + 40, ROW_WIDTH, 16));
    this.poseIdField.setMaxLength(256);
    this.poseIdField.setValue(storedPoseId);

    this.poseSelectionButton =
        this.screen.addActionEntryWidget(
            new SpinButton<>(
                editorLeft,
                editorTop + 20,
                ROW_WIDTH,
                16,
                poseSelection,
                selectedPoseId,
                button -> this.applyPoseSelection(button.get())));
    this.applyPoseSelection(selectedPoseId);
  }

  private void applyPoseSelection(String poseId) {
    boolean customPose = CUSTOM_POSE_ID.equals(poseId);
    this.poseIdField.setVisible(customPose);
    if (!customPose) {
      this.poseIdField.setValue(poseId);
    }
  }

  private boolean isCustomPose() {
    return this.poseSelectionButton != null
        && CUSTOM_POSE_ID.equals(this.poseSelectionButton.get());
  }

  private String getPoseId() {
    if (this.isCustomPose()) {
      return this.poseIdField.getValue().trim();
    }

    return this.poseSelectionButton != null ? this.poseSelectionButton.get() : "";
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.pose",
        editorLeft + 2,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);
  }

  @Override
  public boolean isValid() {
    return Identifier.tryParse(this.getPoseId()) != null;
  }

  @Override
  public boolean hasChanged() {
    return !hasActionData(ActionDataType.SET_POSE)
        || !this.getPoseId().equals(this.actionDataEntry.poseId());
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    return new ActionDataEntry(ActionDataType.SET_POSE).withPoseId(this.getPoseId());
  }
}
