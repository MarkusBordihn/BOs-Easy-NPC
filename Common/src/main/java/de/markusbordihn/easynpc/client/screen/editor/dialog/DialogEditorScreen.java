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

package de.markusbordihn.easynpc.client.screen.editor.dialog;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.EditorScreen;
import de.markusbordihn.easynpc.client.screen.components.AddButton;
import de.markusbordihn.easynpc.client.screen.components.CancelButton;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.client.screen.components.DialogButton;
import de.markusbordihn.easynpc.client.screen.components.DialogButtonButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.SpriteButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextEditButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
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
  protected Button dialogTextButton;
  protected Button addDialogButton;
  protected Button saveButton;
  protected Button cancelButton;
  protected Button deleteButton;
  protected TextField dialogLabelTextField;
  protected Checkbox dialogLabelCheckbox;
  protected Button dialogNameToLabelButton;
  protected TextField dialogNameTextField;
  protected Checkbox dialogTranslateCheckbox;
  private String dialogLabelValue = "";
  private String dialogNameValue = "";
  private boolean dialogTranslateValue = false;

  public DialogEditorScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void openPreviousScreen() {
    NetworkMessageHandlerManager.getServerHandler()
        .openConfiguration(this.getNpcUUID(), ConfigurationType.DIALOG);
  }

  private void deleteDialog() {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed && this.getNpcUUID() != null) {
                NetworkMessageHandlerManager.getServerHandler()
                    .removeDialog(this.getNpcUUID(), this.getDialogUUID());
                NetworkMessageHandlerManager.getServerHandler()
                    .openConfiguration(this.getNpcUUID(), ConfigurationType.DIALOG);
              } else {
                minecraft.setScreen(this);
              }
            },
            Component.translatable(Constants.TEXT_PREFIX + "removeDialog.deleteQuestion"),
            Component.translatable(
                Constants.TEXT_PREFIX + "removeDialog.deleteWarning",
                this.getDialogData().getName()),
            Component.translatable(Constants.TEXT_PREFIX + "removeDialog.deleteButton"),
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

    if (this.dialogTextButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "dialog.text",
          leftPos + 10,
          this.dialogTextButton.getY() - 14,
          Constants.FONT_COLOR_BLACK);
    }

    if (this.dialogTranslateCheckbox != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "dialog.buttons",
          leftPos + 10,
          this.dialogTranslateCheckbox.getY() + 22,
          Constants.FONT_COLOR_BLACK);
    }
  }

  private void saveDialogData() {
    // Check if something has changed, otherwise we don't need to save the dialog data.
    boolean hasChanged =
        !this.dialogNameTextField.getValue().equals(this.dialogNameValue)
            || !this.dialogLabelTextField.getValue().equals(this.dialogLabelValue)
            || this.dialogTranslateCheckbox.selected() != this.dialogTranslateValue;
    if (!hasChanged) {
      return;
    }

    // Define new dialog data
    DialogDataEntry dialogDataEntry = this.getDialogData();
    dialogDataEntry.setName(this.dialogNameTextField.getValue());
    dialogDataEntry.setLabel(this.dialogLabelTextField.getValue());
    dialogDataEntry.setTranslate(this.dialogTranslateCheckbox.selected());

    // Save dialog data
    NetworkMessageHandlerManager.getServerHandler()
        .saveDialog(this.getNpcUUID(), this.getDialogUUID(), dialogDataEntry);
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
                18,
                "<",
                onPress -> this.openPreviousScreen()));

    // Save Button
    this.saveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.leftPos + 25,
                this.bottomPos - 35,
                85,
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
                this.bottomPos - 35,
                85,
                onPress -> this.deleteDialog()));

    // Chancel Button
    this.cancelButton =
        this.addRenderableWidget(
            new CancelButton(
                this.deleteButton.getX() + this.deleteButton.getWidth() + 10,
                this.bottomPos - 35,
                85,
                "cancel",
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
        new TextField(this.font, this.leftPos + 100, this.topPos + 30, 150, this.dialogNameValue);
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
    this.dialogLabelTextField = new TextField(this.font, this.leftPos + 100, this.topPos + 50, 100);
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

    // Dialog Text
    this.dialogTextButton =
        this.addRenderableWidget(
            new TextEditButton(
                this.leftPos + 7,
                this.topPos + 105,
                303,
                "dialog.edit_text",
                onPress -> {
                  // Check if something has changed, to store the current dialog data before opening
                  // the dialog text editor.
                  this.saveDialogData();
                  NetworkMessageHandlerManager.getServerHandler()
                      .openDialogTextEditor(this.getNpcUUID(), this.getDialogUUID());
                }));

    // Dialog Translate
    this.dialogTranslateValue = dialogDataEntry.getTranslate();
    this.dialogTranslateCheckbox =
        new Checkbox(
            this.leftPos + 15, this.topPos + 110, "dialog.translate", this.dialogTranslateValue);
    this.addRenderableWidget(this.dialogTranslateCheckbox);
    this.dialogTranslateCheckbox.visible = false;

    // Dialog Buttons (max. 6 in two rows)
    this.defineDialogButtons(dialogDataEntry);
  }

  private void defineDialogButtons(DialogDataEntry dialogDataEntry) {
    Set<DialogButtonEntry> dialogButtons = dialogDataEntry.getDialogButtons();
    boolean smallButtons = dialogButtons.size() < 4;
    int buttonIndex = 0;
    int buttonBaseLeftPos = this.leftPos + 7;
    int buttonTopPos = this.topPos + 145;
    int buttonLeftPos = buttonBaseLeftPos;
    int buttonSpace = 3;
    int buttonWidth = smallButtons ? 150 : 100;
    int buttonMaxTextLength = smallButtons ? 22 : 14;

    for (DialogButtonEntry dialogButtonEntry : dialogButtons) {
      if (buttonIndex > MAX_NUMBER_OF_BUTTONS - 1) {
        break;
      }
      if ((smallButtons && buttonIndex == 2) || (!smallButtons && buttonIndex == 3)) {
        buttonTopPos += 20;
        buttonLeftPos = buttonBaseLeftPos;
      }
      Button dialogActionButton =
          new DialogButtonButton(
              buttonLeftPos,
              buttonTopPos,
              buttonWidth,
              dialogButtonEntry.getName(buttonMaxTextLength),
              onPress -> {
                log.info("Edit dialog button {}", dialogButtonEntry.getId());
                NetworkMessageHandlerManager.getServerHandler()
                    .openDialogButtonEditor(
                        this.getNpcUUID(), this.getDialogUUID(), dialogButtonEntry.getId());
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
              dialogButtons.size() < 5 && buttonIndex != 2 ? buttonTopPos : buttonTopPos + 20,
              smallButtons ? buttonWidth : 150,
              "dialog.add_button",
              onPress -> {
                // Check if something has changed, to store the current dialog data before opening
                // the dialog button editor.
                this.saveDialogData();
                NetworkMessageHandlerManager.getServerHandler()
                    .openDialogButtonEditor(this.getNpcUUID(), this.getDialogUUID());
              });
      this.addRenderableWidget(this.addDialogButton);
    }
  }

  @Override
  public void containerTick() {
    super.containerTick();

    if (this.saveButton != null) {
      this.saveButton.active =
          !this.dialogNameTextField.getValue().equals(this.dialogNameValue)
              || !this.dialogLabelTextField.getValue().equals(this.dialogLabelValue)
              || this.dialogTranslateCheckbox.selected() != this.dialogTranslateValue;
    }

    if (this.dialogLabelCheckbox != null && this.dialogNameToLabelButton != null) {
      this.dialogNameToLabelButton.active = !this.dialogLabelCheckbox.selected();
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);
    this.renderEditLabels(guiGraphics);

    // Render Tooltips
    if (this.dialogNameToLabelButton != null && this.dialogNameToLabelButton.isMouseOver(x, y)) {
      guiGraphics.renderTooltip(
          this.font,
          Component.translatable(Constants.TEXT_CONFIG_PREFIX + "name_to_label.tooltip"),
          x,
          y);
    }
  }
}
