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

package de.markusbordihn.easynpc.client.screen.editor;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.AddButton;
import de.markusbordihn.easynpc.client.screen.components.CancelButton;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.CloseButton;
import de.markusbordihn.easynpc.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.client.screen.components.DialogButton;
import de.markusbordihn.easynpc.client.screen.components.DialogButtonButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.SpriteButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextEditButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.editor.DialogEditorMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import java.util.Set;
import java.util.UUID;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@OnlyIn(Dist.CLIENT)
public class DialogEditorScreen extends AbstractContainerScreen<DialogEditorMenu> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final int MAX_NUMBER_OF_BUTTONS = 6;

  // General
  protected final ClientLevel clientLevel;
  protected final LocalPlayer localPlayer;
  protected final Minecraft minecraftInstance;

  // Data access
  protected final DialogDataSet dialogDataSet;
  protected final DialogDataEntry dialogData;
  protected final Set<DialogButtonData> dialogButtons;
  protected final EasyNPCEntity entity;
  protected final UUID uuid;
  protected final ConfigurationType formerConfigurationType;
  protected UUID dialogId;

  // Navigation
  protected Button homeButton;
  protected Button dialogButton;

  // Buttons
  protected Button dialogTextButton;
  protected Button addDialogButton;
  protected Button closeButton;
  protected Button saveButton;
  protected Button cancelButton;
  protected Button deleteButton;

  // Edit Boxes
  protected TextField dialogLabelTextField;
  protected Checkbox dialogLabelCheckbox;
  protected Button dialogNameToLabelButton;
  protected TextField dialogNameTextField;
  protected Checkbox dialogTranslateCheckbox;

  // Internal
  protected int bottomPos;
  protected float xMouse;
  protected float yMouse;
  protected int rightPos;

  // Cache
  private String dialogNameValue = "";
  private String dialogLabelValue = "";
  private boolean dialogTranslateValue = false;

  @OnlyIn(Dist.CLIENT)
  public DialogEditorScreen(DialogEditorMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);

    // Data access
    this.uuid = menu.getUUID();
    this.entity = menu.getEntity();
    this.dialogDataSet = menu.getDialogDataSet();
    this.dialogData = menu.getDialogData();
    this.dialogButtons = dialogData.getButtons();
    this.dialogId = menu.getDialogId();
    this.formerConfigurationType = menu.getFormerConfigurationType();

    // General environment Data
    this.minecraftInstance = Minecraft.getInstance();
    this.localPlayer = this.minecraftInstance.player;
    this.clientLevel = this.minecraftInstance.level;
  }

  private void openPreviousScreen() {
    if (this.formerConfigurationType == ConfigurationType.DIALOG_EDITOR) {
      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.ADVANCED_DIALOG);
    } else if (this.formerConfigurationType != null) {
      NetworkMessageHandler.openConfiguration(uuid, this.formerConfigurationType);
    } else if (dialogDataSet.getType() == DialogType.YES_NO) {
      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.YES_NO_DIALOG);
    } else {
      this.closeScreen();
    }
  }

  private void closeScreen() {
    if (this.minecraftInstance != null) {
      this.minecraftInstance.setScreen(null);
    }
  }

  private void deleteDialog() {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed && uuid != null) {
                NetworkMessageHandler.removeDialog(uuid, dialogId);
                this.openPreviousScreen();
              } else {
                minecraft.setScreen(this);
              }
            },
            Component.translatable(Constants.TEXT_PREFIX + "removeDialog.deleteQuestion"),
            Component.translatable(
                Constants.TEXT_PREFIX + "removeDialog.deleteWarning", this.dialogData.getName()),
            Component.translatable(Constants.TEXT_PREFIX + "removeDialog.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  protected void renderEditLabels(GuiGraphics guiGraphics) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "dialog.name",
        leftPos + 10,
        this.dialogNameTextField.getY() + 4,
        Constants.FONT_COLOR_BLACK);

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "label_id",
        leftPos + 10,
        this.dialogLabelTextField.getY() + 4,
        Constants.FONT_COLOR_BLACK);

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "dialog.text",
        leftPos + 10,
        this.dialogTextButton.getY() - 14,
        Constants.FONT_COLOR_BLACK);

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "dialog.buttons",
        leftPos + 10,
        this.dialogTranslateCheckbox.getY() + 22,
        Constants.FONT_COLOR_BLACK);
  }

  protected void saveDialogData() {
    // Check if something has changed, otherwise we don't need to save the dialog data.
    boolean hasChanged =
        !this.dialogNameTextField.getValue().equals(this.dialogNameValue)
            || !this.dialogLabelTextField.getValue().equals(this.dialogLabelValue)
            || this.dialogTranslateCheckbox.selected() != this.dialogTranslateValue;
    if (!hasChanged) {
      return;
    }

    // Define new dialog data
    this.dialogData.setName(this.dialogNameTextField.getValue());
    this.dialogData.setLabel(this.dialogLabelTextField.getValue());
    this.dialogData.setTranslate(this.dialogTranslateCheckbox.selected());

    // Save dialog data
    NetworkMessageHandler.saveDialog(this.uuid, this.dialogId, this.dialogData);

    // Update dialog id, if label has changed.
    if (!this.dialogLabelTextField.getValue().equals(this.dialogLabelValue)) {
      this.dialogId = this.dialogData.getId();
    }
  }

  @Override
  public void init() {
    super.init();

    // Default stats
    this.imageHeight = 243;
    this.imageWidth = 318;

    // Core Positions
    this.titleLabelX = 8;
    this.titleLabelY = 7;
    this.topPos = ((this.height - this.imageHeight) / 2) + 2;
    this.leftPos = (this.width - this.imageWidth) / 2;
    this.rightPos = this.leftPos + this.imageWidth;
    this.bottomPos = this.topPos + this.imageHeight;

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

    // Dialog Button
    this.dialogButton =
        this.addRenderableWidget(
            new DialogButton(
                this.homeButton.getX() + this.homeButton.getWidth(),
                this.topPos + 7,
                140,
                this.dialogData.getName(21),
                onPress -> {}));
    this.dialogButton.active = false;

    // Dialog Name
    this.dialogNameValue = this.dialogData.getName();
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
    this.dialogLabelValue = this.dialogData.getLabel();
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
            !this.dialogData.getLabel().isEmpty(),
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

                  NetworkMessageHandler.openDialogTextEditor(
                      uuid, this.dialogId, ConfigurationType.DIALOG_EDITOR);
                }));

    // Dialog Translate
    this.dialogTranslateValue = this.dialogData.getTranslate();
    this.dialogTranslateCheckbox =
        new Checkbox(
            this.leftPos + 15, this.topPos + 110, "dialog.translate", this.dialogTranslateValue);
    this.addRenderableWidget(this.dialogTranslateCheckbox);
    this.dialogTranslateCheckbox.visible = false;

    // Dialog Buttons (max. 6 in two rows)
    boolean smallButtons = this.dialogButtons.size() < 4;
    int buttonIndex = 0;
    int buttonBaseLeftPos = this.leftPos + 7;
    int buttonTopPos = this.topPos + 145;
    int buttonLeftPos = buttonBaseLeftPos;
    int buttonSpace = 3;
    int buttonWidth = smallButtons ? 150 : 100;
    int buttonMaxTextLength = smallButtons ? 22 : 14;

    for (DialogButtonData dialogButtonData : this.dialogButtons) {
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
              dialogButtonData.getName(buttonMaxTextLength),
              onPress -> {
                log.info("Edit dialog button {}", dialogButtonData.getId());
                NetworkMessageHandler.openDialogButtonEditor(
                    uuid, this.dialogId, dialogButtonData.getId(), ConfigurationType.DIALOG_EDITOR);
              });
      this.addRenderableWidget(dialogActionButton);
      buttonLeftPos += buttonWidth + buttonSpace;
      buttonIndex++;
    }

    // Add Dialog Button if less than 6 buttons
    if (buttonIndex < MAX_NUMBER_OF_BUTTONS) {
      this.addDialogButton =
          new AddButton(
              this.dialogButtons.size() < 5 && buttonIndex != 2 ? buttonLeftPos : buttonBaseLeftPos,
              this.dialogButtons.size() < 5 && buttonIndex != 2 ? buttonTopPos : buttonTopPos + 20,
              smallButtons ? buttonWidth : 150,
              "dialog.add_button",
              onPress -> {
                // Check if something has changed, to store the current dialog data before opening
                // the dialog button editor.
                this.saveDialogData();

                NetworkMessageHandler.openDialogButtonEditor(
                    uuid, this.dialogId, ConfigurationType.DIALOG_EDITOR);
              });
      this.addRenderableWidget(this.addDialogButton);
    }

    // Close Button
    this.closeButton =
        this.addRenderableWidget(
            new CloseButton(this.rightPos - 15, this.topPos + 4, onPress -> closeScreen()));

    // Save Button
    this.saveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.leftPos + 25,
                this.bottomPos - 35,
                85,
                "save",
                onPress -> {
                  // Save dialog data
                  this.saveDialogData();

                  // Return back to the simple yes and no dialog editor or the full dialog editor.
                  openPreviousScreen();
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
    this.renderBackground(guiGraphics);
    super.render(guiGraphics, x, y, partialTicks);
    this.renderEditLabels(guiGraphics);
    this.xMouse = x;
    this.yMouse = y;

    // Render Tooltips
    if (this.dialogNameToLabelButton.isMouseOver(x, y)) {
      guiGraphics.renderTooltip(
          this.font,
          Component.translatable(Constants.TEXT_CONFIG_PREFIX + "name_to_label.tooltip"),
          x,
          y);
    }
  }

  @Override
  protected void renderLabels(GuiGraphics guiGraphics, int x, int y) {}

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    // Main screen: top left
    guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos, 0, 0, 210, 160);

    // Main screen: top right
    guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 203, topPos, 132, 0, 120, 160);

    // Main screen: bottom left
    guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos + 77, 0, 5, 210, 170);

    // Main screen: bottom right
    guiGraphics.blit(
        Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 203, topPos + 77, 132, 5, 120, 170);
  }

  @Override
  public boolean keyPressed(int keyCode, int unused1, int unused2) {
    if (keyCode != 257 && keyCode != 335 && keyCode != 69 && keyCode != 73) {
      return super.keyPressed(keyCode, unused1, unused2);
    }
    return keyCode == 257 || keyCode == 335 || keyCode == 73;
  }
}
