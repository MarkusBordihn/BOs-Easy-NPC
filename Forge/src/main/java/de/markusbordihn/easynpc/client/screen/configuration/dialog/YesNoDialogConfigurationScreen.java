/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.easynpc.client.screen.configuration.dialog;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.CancelButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.configuration.dialog.YesNoDialogConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.Set;
import java.util.UUID;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class YesNoDialogConfigurationScreen
    extends DialogConfigurationScreen<YesNoDialogConfigurationMenu> {

  private static final String YES_BUTTON_LABEL = "yes_button";
  private static final String NO_BUTTON_LABEL = "no_button";

  // Dialog data
  private final DialogDataEntry questionDialogData;
  private final DialogDataEntry yesDialogData;
  private final DialogDataEntry noDialogData;

  // Question Dialog
  protected EditBox mainDialogBox;
  protected Button yesDialogButton;
  protected Button noDialogButton;

  // Yes Dialog
  protected EditBox yesDialogBox;

  // No Dialog
  protected EditBox noDialogBox;

  // Buttons
  protected Button saveButton = null;
  protected Button cancelButton = null;

  // Cache
  boolean showSaveNotificationForButtons = false;
  private String questionDialogValue = "";
  private String yesDialogValue = "";
  private String noDialogValue = "";

  public YesNoDialogConfigurationScreen(
      YesNoDialogConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.questionDialogData = this.dialogDataSet.getDialog("question");
    this.yesDialogData = this.dialogDataSet.getDialog("yes_answer");
    this.noDialogData = this.dialogDataSet.getDialog("no_answer");
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.yesNoDialogButton.active = false;

    // Dialog Buttons
    DialogButtonData yesButtonData =
        this.questionDialogData == null
            ? null
            : this.questionDialogData.getButton(YES_BUTTON_LABEL);
    DialogButtonData noButtonData =
        this.questionDialogData == null ? null : this.questionDialogData.getButton(NO_BUTTON_LABEL);

    // Question Text (copy from basic text if not set)
    this.questionDialogValue =
        this.questionDialogData == null ? "" : this.questionDialogData.getText();
    if (this.questionDialogValue.isEmpty()
        && this.dialogDataSet.getDefaultDialog() != null
        && this.dialogDataSet.getType() == DialogType.BASIC) {
      this.questionDialogValue = this.dialogDataSet.getDefaultDialog().getText();
    }

    // Save notification for buttons
    if (this.questionDialogData == null) {
      this.showSaveNotificationForButtons = true;
    }

    // Dialog
    this.mainDialogBox = new TextField(this.font, this.contentLeftPos, this.topPos + 50, 300);
    this.mainDialogBox.setMaxLength(255);
    this.mainDialogBox.setValue(this.questionDialogValue);
    this.addRenderableWidget(this.mainDialogBox);

    // Question Dialog Buttons
    Component yesButtonText =
        Component.literal(
            TextUtils.limitString(
                yesButtonData == null ? "Yes Button" : yesButtonData.getName(), 18));
    this.yesDialogButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos,
                this.topPos + 85,
                145,
                yesButtonText,
                onPress -> {
                  UUID yesButtonId = yesButtonData == null ? null : yesButtonData.getId();
                  if (this.questionDialogData != null) {
                    if (yesButtonId != null) {
                      NetworkMessageHandler.openDialogButtonEditor(
                          uuid,
                          this.questionDialogData.getId(),
                          yesButtonId,
                          ConfigurationType.YES_NO_DIALOG);
                    } else {
                      NetworkMessageHandler.openDialogButtonEditor(
                          uuid, this.questionDialogData.getId(), ConfigurationType.YES_NO_DIALOG);
                    }
                  }
                }));

    Component noButtonText =
        Component.literal(
            TextUtils.limitString(noButtonData == null ? "No Button" : noButtonData.getName(), 18));
    this.noDialogButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos + 155,
                this.topPos + 85,
                145,
                noButtonText,
                onPress -> {
                  UUID noButtonId = noButtonData == null ? null : noButtonData.getId();
                  if (this.questionDialogData != null) {
                    if (noButtonId != null) {
                      NetworkMessageHandler.openDialogButtonEditor(
                          uuid,
                          this.questionDialogData.getId(),
                          noButtonId,
                          ConfigurationType.YES_NO_DIALOG);
                    } else {
                      NetworkMessageHandler.openDialogButtonEditor(
                          uuid, this.questionDialogData.getId(), ConfigurationType.YES_NO_DIALOG);
                    }
                  }
                }));

    // Yes Dialog
    this.yesDialogValue = yesDialogData == null ? "" : yesDialogData.getText();
    this.yesDialogBox = new TextField(this.font, this.contentLeftPos, this.topPos + 130, 300);
    this.yesDialogBox.setMaxLength(255);
    this.yesDialogBox.setValue(this.yesDialogValue);
    this.addRenderableWidget(this.yesDialogBox);

    // No Dialog
    this.noDialogValue = noDialogData == null ? "" : noDialogData.getText();
    this.noDialogBox = new TextField(this.font, this.contentLeftPos, this.topPos + 170, 300);
    this.noDialogBox.setMaxLength(255);
    this.noDialogBox.setValue(this.noDialogValue);
    this.addRenderableWidget(this.noDialogBox);

    // Save Button
    this.saveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.contentLeftPos + 26,
                this.bottomPos - 40,
                "save",
                onPress -> {
                  DialogDataSet dialogDataSet =
                      DialogUtils.getYesNoDialog(
                          this.mainDialogBox.getValue(),
                          "Yes",
                          "No",
                          this.yesDialogBox.getValue(),
                          this.noDialogBox.getValue());

                  // Check if we have a question dialog and add yes/no buttons if not available.
                  if (questionDialogData != null) {
                    Set<DialogButtonData> dialogButtonDataSet = questionDialogData.getButtons();

                    // Double-check that we have valid yes_no buttons, in the case the user
                    // renamed the labels to something else and not to get lost.
                    if (!questionDialogData.hasButton(YES_BUTTON_LABEL)) {
                      dialogButtonDataSet.add(
                          dialogDataSet.getDialog("question").getButton(YES_BUTTON_LABEL));
                    }
                    if (!questionDialogData.hasButton(NO_BUTTON_LABEL)) {
                      dialogButtonDataSet.add(
                          dialogDataSet.getDialog("question").getButton(NO_BUTTON_LABEL));
                    }

                    // Update dialog buttons for question dialog
                    dialogDataSet.getDialog("question").setButtons(dialogButtonDataSet);
                  }

                  this.questionDialogValue = this.mainDialogBox.getValue();
                  this.yesDialogValue = this.yesDialogBox.getValue();
                  this.noDialogValue = this.noDialogBox.getValue();
                  NetworkMessageHandler.saveDialog(uuid, dialogDataSet);
                  NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.YES_NO_DIALOG);
                }));

    // Chancel Button
    this.cancelButton =
        this.addRenderableWidget(
            new CancelButton(
                this.rightPos - 130,
                this.bottomPos - 40,
                "cancel",
                onPress -> this.showMainScreen()));
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Edit box Labels
    Text.drawConfigString(
        guiGraphics, this.font, "question", this.contentLeftPos, this.topPos + 40);

    Text.drawConfigString(
        guiGraphics, this.font, "edit_yes_button", this.contentLeftPos, this.topPos + 75);

    Text.drawConfigString(
        guiGraphics, this.font, "edit_no_button", this.contentLeftPos + 155, this.topPos + 75);

    Text.drawConfigString(
        guiGraphics, this.font, "yes_answer", this.contentLeftPos, this.yesDialogBox.getY() - 12);

    Text.drawConfigString(
        guiGraphics, this.font, "no_answer", this.contentLeftPos, this.noDialogBox.getY() - 12);

    // Save notification
    if (this.showSaveNotificationForButtons) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "save_before_edit_buttons",
          this.contentLeftPos + 65,
          this.topPos + 105,
          Constants.FONT_COLOR_RED);
    }
  }

  @Override
  public void containerTick() {
    super.containerTick();

    if (this.saveButton != null) {
      this.saveButton.active =
          !this.mainDialogBox.getValue().equals(this.questionDialogValue)
              || !this.yesDialogBox.getValue().equals(this.yesDialogValue)
              || !this.noDialogBox.getValue().equals(this.noDialogValue);
    }

    if (yesDialogButton != null && this.dialogDataSet != null) {
      yesDialogButton.active =
          this.dialogDataSet.hasDialog("question")
              && this.dialogDataSet.getDialog("question").hasButton(YES_BUTTON_LABEL);
    }

    if (noDialogButton != null && this.dialogDataSet != null) {
      noDialogButton.active =
          this.dialogDataSet.hasDialog("question")
              && this.dialogDataSet.getDialog("question").hasButton(NO_BUTTON_LABEL);
    }
  }
}
