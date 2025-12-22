/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.client.screen.editor.dialog;

import de.markusbordihn.easynpc.client.screen.components.AddButton;
import de.markusbordihn.easynpc.client.screen.components.CancelButton;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.SpriteButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextEditButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.EditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.components.DialogButton;
import de.markusbordihn.easynpc.configui.client.screen.components.DialogButtonButton;
import de.markusbordihn.easynpc.configui.client.screen.components.DialogPriorityButton;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.Set;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class DialogEditorScreen<T extends EditorMenu> extends EditorScreen<T> {

  private static final int MAX_NUMBER_OF_BUTTONS = 6;
  protected Button homeButton;
  protected Button dialogButton;
  protected Button conditionsButton;
  protected Button dialogTextButton;
  protected Button addDialogButton;
  protected Button saveButton;
  protected Button cancelButton;
  protected Button deleteButton;
  protected TextField dialogLabelTextField;
  protected Checkbox dialogLabelCheckbox;
  protected Button dialogNameToLabelButton;
  protected DialogPriorityButton dialogPriorityButton;
  protected TextField dialogPriorityTextField;
  protected TextField dialogNameTextField;
  private String dialogLabelValue = "";
  private String dialogNameValue = "";
  private int dialogPriorityValue = 0;

  public DialogEditorScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void openPreviousScreen() {
    NetworkMessageHandlerManager.getServerHandler()
        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.DIALOG);
  }

  private void deleteDialog() {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed && this.getEasyNPCUUID() != null) {
                NetworkMessageHandlerManager.getServerHandler()
                    .removeDialog(this.getEasyNPCUUID(), this.getDialogUUID());
                NetworkMessageHandlerManager.getServerHandler()
                    .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.DIALOG);
              } else {
                minecraft.setScreen(this);
              }
            },
            TextComponent.getTranslatedConfigText("removeDialog.deleteQuestion"),
            TextComponent.getTranslatedConfigText(
                "removeDialog.deleteWarning", this.getDialogData().getName()),
            TextComponent.getTranslatedConfigText("removeDialog.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  private void renderEditLabels(GuiGraphics guiGraphics) {
    if (this.dialogNameTextField != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "dialog.name",
          leftPos + 10,
          this.dialogNameTextField.getY() + 4,
          Constants.FONT_COLOR_BLACK);
    }

    if (this.dialogLabelTextField != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "label_id",
          leftPos + 10,
          this.dialogLabelTextField.getY() + 4,
          Constants.FONT_COLOR_BLACK);
    }

    if (this.dialogPriorityButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "dialog.priority",
          leftPos + 10,
          this.dialogPriorityButton.getY() + 4,
          Constants.FONT_COLOR_BLACK);
    }

    if (this.conditionsButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "dialog.conditions",
          leftPos + 10,
          this.conditionsButton.getY() + 4,
          Constants.FONT_COLOR_BLACK);
    }

    if (this.dialogTextButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "dialog.text",
          leftPos + 10,
          this.dialogTextButton.getY() - 12,
          Constants.FONT_COLOR_BLACK);

      Text.drawConfigString(
          guiGraphics,
          this.font,
          "dialog.buttons",
          leftPos + 10,
          this.dialogTextButton.getY() + 25,
          Constants.FONT_COLOR_BLACK);
    }
  }

  private void saveDialogData() {
    int currentPriority = this.dialogPriorityButton.getPriority();
    if (this.dialogPriorityButton.isCustom() && this.dialogPriorityTextField != null) {
      try {
        currentPriority = Integer.parseInt(this.dialogPriorityTextField.getValue());
      } catch (NumberFormatException ignored) {
      }
    }

    boolean hasChanged =
        !this.dialogNameTextField.getValue().equals(this.dialogNameValue)
            || !this.dialogLabelTextField.getValue().equals(this.dialogLabelValue)
            || currentPriority != this.dialogPriorityValue;
    if (!hasChanged) {
      return;
    }

    DialogDataEntry dialogDataEntry = this.getDialogData();
    dialogDataEntry.setName(this.dialogNameTextField.getValue());
    dialogDataEntry.setLabel(this.dialogLabelTextField.getValue());
    dialogDataEntry.setPriority(currentPriority);

    NetworkMessageHandlerManager.getServerHandler()
        .saveDialog(this.getEasyNPCUUID(), this.getDialogUUID(), dialogDataEntry);
  }

  @Override
  public void init() {
    super.init();

    // Home Button
    this.homeButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + 7,
                this.topPos + 7,
                10,
                16,
                "<",
                onPress -> this.openPreviousScreen()));

    // Dialog Data
    DialogDataEntry dialogDataEntry = this.getDialogData();

    // Dialog Button
    this.dialogButton =
        this.addRenderableWidget(
            new DialogButton(
                this.homeButton.getX() + this.homeButton.getWidth(),
                this.topPos + 7,
                140,
                dialogDataEntry.getName(21),
                onPress -> {}));
    this.dialogButton.active = false;

    // Dialog Name
    this.dialogNameValue = this.getDialogData().getName();
    this.dialogNameTextField =
        new TextField(this.font, this.leftPos + 100, this.topPos + 28, 150, this.dialogNameValue);
    this.dialogNameTextField.setMaxLength(64);
    this.addRenderableWidget(this.dialogNameTextField);

    // Convert Dialog Name to Dialog Label
    this.dialogNameToLabelButton =
        this.addRenderableWidget(
            new SpriteButton(
                this.dialogNameTextField.getX() + this.dialogNameTextField.getWidth() + 1,
                this.dialogNameTextField.getY() - 1,
                18,
                18,
                4,
                4,
                80,
                80,
                12,
                12,
                onPress -> {
                  if (this.dialogNameTextField != null && this.dialogLabelTextField != null) {
                    String buttonName = this.dialogNameTextField.getValue();
                    this.dialogLabelTextField.setValue(DialogUtils.generateButtonLabel(buttonName));
                  }
                }));

    // Dialog Label
    this.dialogLabelValue = dialogDataEntry.getLabel();
    this.dialogLabelTextField = new TextField(this.font, this.leftPos + 100, this.topPos + 48, 100);
    this.dialogLabelTextField.setMaxLength(DialogDataEntry.MAX_DIALOG_LABEL_LENGTH);
    this.dialogLabelTextField.setValue(this.dialogLabelValue);
    this.dialogLabelTextField.setEditable(this.dialogLabelTextField.getValue().isEmpty());
    this.addRenderableWidget(this.dialogLabelTextField);

    // Lock Button Label for DialogId, if not empty to prevent accidental changes.
    this.dialogLabelCheckbox =
        new Checkbox(
            this.leftPos + 203,
            this.dialogLabelTextField.getY() + 1,
            "locked",
            !dialogDataEntry.getLabel().isEmpty(),
            checkbox -> this.dialogLabelTextField.setEditable(!checkbox.selected()));
    this.addRenderableWidget(this.dialogLabelCheckbox);

    // Conditions Button
    this.conditionsButton =
        this.addRenderableWidget(
            new DialogButton(
                this.leftPos + 99,
                this.topPos + 67,
                140,
                dialogDataEntry.hasConditions()
                    ? "dialog.edit_conditions"
                    : "dialog.create_conditions",
                onPress -> {
                  this.saveDialogData();
                  NetworkMessageHandlerManager.getServerHandler()
                      .openConditionDataEditor(this.getEasyNPCUUID(), this.getDialogUUID());
                }));

    // Dialog Priority
    this.dialogPriorityValue = dialogDataEntry.getPriority();
    this.dialogPriorityButton =
        this.addRenderableWidget(
            new DialogPriorityButton(
                this.leftPos + 99,
                this.topPos + 86,
                110,
                16,
                this.dialogPriorityValue,
                button -> {
                  if (this.dialogPriorityTextField != null) {
                    this.dialogPriorityTextField.setVisible(this.dialogPriorityButton.isCustom());
                    if (this.dialogPriorityButton.isCustom()
                        && this.dialogPriorityTextField.getValue().isEmpty()) {
                      this.dialogPriorityTextField.setValue(
                          String.valueOf(this.dialogPriorityValue));
                    }
                  }
                }));

    // Custom Priority TextField
    this.dialogPriorityTextField =
        new TextField(
            this.font,
            this.dialogPriorityButton.getX() + this.dialogPriorityButton.getWidth() + 2,
            this.dialogPriorityButton.getY(),
            30,
            this.dialogPriorityButton.isCustom() ? String.valueOf(this.dialogPriorityValue) : "",
            3);
    this.dialogPriorityTextField.setVisible(this.dialogPriorityButton.isCustom());
    this.addRenderableWidget(this.dialogPriorityTextField);

    // Dialog Text
    this.dialogTextButton =
        this.addRenderableWidget(
            new TextEditButton(
                this.leftPos + 7,
                this.topPos + 122,
                315,
                "dialog.edit_text",
                onPress -> {
                  // Check if something has changed, to store the current dialog data before opening
                  // the dialog text editor.
                  this.saveDialogData();
                  NetworkMessageHandlerManager.getServerHandler()
                      .openDialogTextEditor(this.getEasyNPCUUID(), this.getDialogUUID());
                }));

    // Dialog Buttons (max. 6 in two rows)
    this.defineDialogButtons(dialogDataEntry);

    // Save Button
    this.saveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.leftPos + 8,
                this.bottomPos - 30,
                130,
                "save",
                onPress -> {
                  this.saveDialogData();
                  this.openPreviousScreen();
                }));

    // Delete Button
    this.deleteButton =
        this.addRenderableWidget(
            new DeleteButton(
                this.saveButton.getX() + this.saveButton.getWidth() + 10,
                this.bottomPos - 30,
                85,
                onPress -> this.deleteDialog()));

    // Chancel Button
    this.cancelButton =
        this.addRenderableWidget(
            new CancelButton(
                this.deleteButton.getX() + this.deleteButton.getWidth() + 5,
                this.bottomPos - 30,
                85,
                "cancel",
                onPress -> this.openPreviousScreen()));
  }

  private void defineDialogButtons(DialogDataEntry dialogDataEntry) {
    Set<DialogButtonEntry> dialogButtons = dialogDataEntry.getDialogButtons();
    boolean smallButtons = dialogButtons.size() < 4;
    int buttonIndex = 0;
    int buttonBaseLeftPos = this.leftPos + 7;
    int buttonTopPos = this.topPos + 157;
    int buttonLeftPos = buttonBaseLeftPos;
    int buttonSpace = 3;
    int buttonWidth = smallButtons ? 156 : 103;
    int buttonMaxTextLength = smallButtons ? 22 : 14;

    for (DialogButtonEntry dialogButtonEntry : dialogButtons) {
      if (buttonIndex > MAX_NUMBER_OF_BUTTONS - 1) {
        break;
      }
      if ((smallButtons && buttonIndex == 2) || (!smallButtons && buttonIndex == 3)) {
        buttonTopPos += 18;
        buttonLeftPos = buttonBaseLeftPos;
      }
      Button dialogActionButton =
          new DialogButtonButton(
              buttonLeftPos,
              buttonTopPos,
              buttonWidth,
              dialogButtonEntry.getButtonName(buttonMaxTextLength).getString(),
              onPress -> {
                log.info("Edit dialog button {}", dialogButtonEntry.id());
                NetworkMessageHandlerManager.getServerHandler()
                    .openDialogButtonEditor(
                        this.getEasyNPCUUID(), this.getDialogUUID(), dialogButtonEntry.id());
              });
      this.addRenderableWidget(dialogActionButton);
      buttonLeftPos += buttonWidth + buttonSpace;
      buttonIndex++;
    }

    // Add Dialog Button if less than 6 buttons
    if (buttonIndex < MAX_NUMBER_OF_BUTTONS) {
      this.addDialogButton =
          new AddButton(
              dialogButtons.size() < 5 && buttonIndex != 2 ? buttonLeftPos : buttonBaseLeftPos,
              dialogButtons.size() < 5 && buttonIndex != 2 ? buttonTopPos : buttonTopPos + 18,
              smallButtons ? buttonWidth : 208,
              "dialog.add_button",
              onPress -> {
                // Check if something has changed, to store the current dialog data before opening
                // the dialog button editor.
                this.saveDialogData();
                NetworkMessageHandlerManager.getServerHandler()
                    .openDialogButtonEditor(this.getEasyNPCUUID(), this.getDialogUUID());
              });
      this.addRenderableWidget(this.addDialogButton);
    }
  }

  @Override
  public void updateTick() {
    super.updateTick();

    if (this.saveButton != null) {
      int currentPriority = this.dialogPriorityButton.getPriority();
      if (this.dialogPriorityButton.isCustom() && this.dialogPriorityTextField != null) {
        try {
          currentPriority = Integer.parseInt(this.dialogPriorityTextField.getValue());
        } catch (NumberFormatException ignored) {
        }
      }

      this.saveButton.active =
          !this.dialogNameTextField.getValue().equals(this.dialogNameValue)
              || !this.dialogLabelTextField.getValue().equals(this.dialogLabelValue)
              || currentPriority != this.dialogPriorityValue;
    }

    if (this.dialogLabelCheckbox != null && this.dialogNameToLabelButton != null) {
      this.dialogNameToLabelButton.active = !this.dialogLabelCheckbox.selected();
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);
    this.renderEditLabels(guiGraphics);

    if (this.dialogNameToLabelButton != null && this.dialogNameToLabelButton.isMouseOver(x, y)) {
      guiGraphics.renderTooltip(
          this.font, TextComponent.getTranslatedConfigText("name_to_label.tooltip"), x, y);
    }
  }
}
