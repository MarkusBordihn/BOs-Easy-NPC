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

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.EditorScreen;
import de.markusbordihn.easynpc.client.screen.components.CancelButton;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.client.screen.components.DialogButton;
import de.markusbordihn.easynpc.client.screen.components.DialogButtonButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.SpriteButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import java.util.HashSet;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.player.Inventory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DialogButtonEditorScreen<T extends EditorMenu> extends EditorScreen<T> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected Button homeButton;
  protected Button dialogButton;
  protected Button dialogButtonButton;
  protected Button saveButton;
  protected Button cancelButton;
  protected Button deleteButton;
  protected TextField buttonNameBox;
  protected Button buttonNameToLabelButton;
  protected TextField buttonLabelBox;
  protected Checkbox buttonLabelCheckbox;
  protected TextField openNamedDialogBox;
  protected Checkbox openTradingScreenCheckbox;
  protected TextField commandDialogBox;
  protected Checkbox commandDialogExecuteAsUserCheckbox;
  protected Checkbox commandDialogDebugCheckbox;
  private String buttonLabelValue = "";
  // Cache
  private String buttonNameValue = "";
  private boolean commandDialogDebugValue = false;
  private boolean commandDialogExecuteAsUserValue = false;
  private String commandDialogValue = "";
  private String openNamedDialogValue = "";
  private boolean openTradingScreenValue = false;

  public DialogButtonEditorScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void openPreviousScreen() {
    NetworkMessageHandlerManager.getServerHandler()
        .openConfiguration(this.getNpcUUID(), ConfigurationType.DIALOG);
  }

  private void deleteDialogButton() {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed && getDialogUUID() != null) {
                NetworkMessageHandlerManager.getServerHandler()
                    .removeDialogButton(getNpcUUID(), getDialogUUID(), getDialogButtonUUID());
                this.openPreviousScreen();
              } else {
                minecraft.setScreen(this);
              }
            },
            new TranslatableComponent(Constants.TEXT_PREFIX + "removeDialogButton.deleteQuestion"),
            new TranslatableComponent(
                Constants.TEXT_PREFIX + "removeDialogButton.deleteWarning",
                this.getDialogButtonData().getName()),
            new TranslatableComponent(Constants.TEXT_PREFIX + "removeDialogButton.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  protected void renderEditLabels(PoseStack poseStack) {

    if (this.buttonNameBox != null) {
      Text.drawConfigString(
          poseStack, this.font, "button.name", leftPos + 12, this.buttonNameBox.y + 4);
    }

    if (this.buttonLabelBox != null) {
      Text.drawConfigString(
          poseStack, this.font, "label_id", leftPos + 12, this.buttonLabelBox.y + 4);
    }

    if (this.openNamedDialogBox != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "dialog_actions_on_click",
          leftPos + 12,
          this.openNamedDialogBox.y - 16,
          Constants.FONT_COLOR_BLACK);

      Text.drawConfigString(
          poseStack, this.font, "open_named_dialog", leftPos + 12, this.openNamedDialogBox.y + 4);
    }

    if (this.commandDialogBox != null) {
      Text.drawString(poseStack, this.font, "Actions", leftPos + 12, this.commandDialogBox.y - 10);
    }
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

    // Dialog Button Data
    ActionDataEntry openNamedDialogAction =
        this.getDialogButtonData().getActionData(ActionType.OPEN_NAMED_DIALOG);
    ActionDataEntry openTradingScreenAction =
        this.getDialogButtonData().getActionData(ActionType.OPEN_TRADING_SCREEN);
    ActionDataEntry commandAction = this.getDialogButtonData().getActionData(ActionType.COMMAND);

    // Dialog Button
    this.dialogButton =
        this.addRenderableWidget(
            new DialogButton(
                this.homeButton.x + this.homeButton.getWidth(),
                this.topPos + 7,
                140,
                this.getDialogData().getName(21),
                onPress -> this.openPreviousScreen()));

    // Dialog Button Button
    this.dialogButtonButton =
        this.addRenderableWidget(
            new DialogButtonButton(
                this.dialogButton.x + this.dialogButton.getWidth(),
                this.topPos + 7,
                140,
                this.getDialogButtonData().getName(21),
                onPress -> {}));
    this.dialogButtonButton.active = false;

    // Button Name
    this.buttonNameValue = this.getDialogButtonData().getName();
    this.buttonNameBox = new TextField(this.font, this.leftPos + 100, this.topPos + 30, 150);
    this.buttonNameBox.setMaxLength(64);
    this.buttonNameBox.setValue(this.buttonNameValue);
    this.addRenderableWidget(this.buttonNameBox);

    // Convert Button Name to Button Label
    this.buttonNameToLabelButton =
        this.addRenderableWidget(
            new SpriteButton(
                this.buttonNameBox.x + this.buttonNameBox.getWidth() + 1,
                this.buttonNameBox.y - 1,
                18,
                18,
                4,
                4,
                80,
                80,
                12,
                12,
                onPress -> {
                  if (this.buttonNameBox != null && this.buttonLabelBox != null) {
                    String buttonName = this.buttonNameBox.getValue();
                    this.buttonLabelBox.setValue(DialogUtils.generateButtonLabel(buttonName));
                  }
                }));

    // Button Label
    this.buttonLabelValue = this.getDialogButtonData().getLabel();
    this.buttonLabelBox = new TextField(this.font, this.leftPos + 100, this.topPos + 50, 100);
    this.buttonLabelBox.setMaxLength(DialogButtonData.MAX_BUTTON_LABEL_LENGTH);
    this.buttonLabelBox.setValue(this.buttonLabelValue);
    this.buttonLabelBox.setEditable(false);
    this.addRenderableWidget(this.buttonLabelBox);

    // Lock Button Label by default to prevent accidental changes.
    this.buttonLabelCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.leftPos + 204,
                this.buttonLabelBox.y + 1,
                "locked",
                true,
                onPress -> this.buttonLabelBox.setEditable(!this.buttonLabelCheckbox.selected())));

    // Open Named Dialog
    this.openNamedDialogValue =
        openNamedDialogAction != null ? openNamedDialogAction.getCommand() : "";
    this.openNamedDialogBox = new TextField(this.font, this.leftPos + 100, this.topPos + 90, 100);
    this.openNamedDialogBox.setMaxLength(32);
    this.openNamedDialogBox.setValue(this.openNamedDialogValue);
    this.openNamedDialogBox.setEditable(
        openTradingScreenAction == null || openTradingScreenAction.getCommand() == null);
    this.addRenderableWidget(this.openNamedDialogBox);

    // Trading Screen Checkbox which will overwrite the open named dialog.
    this.openTradingScreenValue =
        openTradingScreenAction != null && openTradingScreenAction.getCommand() != null;
    this.openTradingScreenCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.leftPos + 100 + 104,
                this.openNamedDialogBox.y + 1,
                "open_trading_screen",
                this.openTradingScreenValue,
                onPress ->
                    this.openNamedDialogBox.setEditable(
                        !this.openTradingScreenCheckbox.selected())));

    // Command Dialog
    this.commandDialogValue = commandAction != null ? commandAction.getCommand() : "";
    this.commandDialogBox = new TextField(this.font, this.leftPos + 10, this.topPos + 130, 295);
    this.commandDialogBox.setMaxLength(255);
    this.commandDialogBox.setValue(this.commandDialogValue);
    this.addRenderableWidget(this.commandDialogBox);

    this.commandDialogExecuteAsUserValue =
        commandAction != null && commandAction.shouldExecuteAsUser();
    this.commandDialogExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.leftPos + 10 + 80,
                commandDialogBox.y + 18,
                "execute_as_player",
                commandDialogExecuteAsUserValue));

    this.commandDialogDebugValue = commandAction != null && commandAction.isDebugEnabled();
    this.commandDialogDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.leftPos + 10 + 215,
                commandDialogBox.y + 18,
                "debug",
                this.commandDialogDebugValue));

    // Save Button
    this.saveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.leftPos + 25,
                this.bottomPos - 35,
                85,
                "save",
                onPress -> {
                  this.saveDialogButton();
                  this.openPreviousScreen();
                }));

    // Delete Button
    this.deleteButton =
        this.addRenderableWidget(
            new DeleteButton(
                this.saveButton.x + this.saveButton.getWidth() + 10,
                this.bottomPos - 35,
                85,
                onPress -> this.deleteDialogButton()));

    // Chancel Button
    this.cancelButton =
        this.addRenderableWidget(
            new CancelButton(
                this.deleteButton.x + this.deleteButton.getWidth() + 10,
                this.bottomPos - 35,
                85,
                "cancel",
                onPress -> this.openPreviousScreen()));
  }

  private void saveDialogButton() {
    DialogButtonData dialogButtonData = this.getDialogButtonData();
    if (dialogButtonData == null) {
      return;
    }

    // Basic dialog button data
    dialogButtonData.setName(this.buttonNameBox.getValue());
    dialogButtonData.setLabel(this.buttonLabelBox.getValue());

    // Action data set
    HashSet<ActionDataEntry> actionDataEntrySet = new HashSet<>();

    // Open named dialog or trading screen
    if (this.openTradingScreenCheckbox.selected()) {
      ActionDataEntry openTradingScreenActionDataEntry =
          new ActionDataEntry(ActionType.OPEN_TRADING_SCREEN);
      actionDataEntrySet.add(openTradingScreenActionDataEntry);
    } else if (!this.openNamedDialogBox.getValue().isEmpty()) {
      ActionDataEntry openNamedDialogActionDataEntry =
          new ActionDataEntry(ActionType.OPEN_NAMED_DIALOG, this.openNamedDialogBox.getValue());
      actionDataEntrySet.add(openNamedDialogActionDataEntry);
    }

    // Other action commands
    ActionDataEntry commandActionDataEntry =
        new ActionDataEntry(
            ActionType.COMMAND,
            this.commandDialogBox.getValue(),
            this.commandDialogExecuteAsUserCheckbox.selected(),
            this.commandDialogDebugCheckbox.selected());
    actionDataEntrySet.add(commandActionDataEntry);
    dialogButtonData.setActionData(actionDataEntrySet);

    // Save dialog button action data.
    NetworkMessageHandlerManager.getServerHandler()
        .saveDialogButton(
            this.getNpcUUID(), this.getDialogUUID(), this.getDialogButtonUUID(), dialogButtonData);
  }

  @Override
  public void containerTick() {
    super.containerTick();

    if (this.saveButton != null) {
      this.saveButton.active =
          !this.buttonNameBox.getValue().equals(this.buttonNameValue)
              || !this.buttonLabelBox.getValue().equals(this.buttonLabelValue)
              || !this.openNamedDialogBox.getValue().equals(this.openNamedDialogValue)
              || !this.commandDialogBox.getValue().equals(this.commandDialogValue)
              || this.openTradingScreenCheckbox.selected() != this.openTradingScreenValue
              || this.commandDialogExecuteAsUserCheckbox.selected()
                  != this.commandDialogExecuteAsUserValue
              || this.commandDialogDebugCheckbox.selected() != this.commandDialogDebugValue;
    }

    if (this.buttonLabelCheckbox != null && this.buttonNameToLabelButton != null) {
      this.buttonNameToLabelButton.active = !this.buttonLabelCheckbox.selected();
    }
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    this.renderBackground(poseStack);
    super.render(poseStack, x, y, partialTicks);
    this.renderEditLabels(poseStack);

    // Render Tooltips
    if (this.buttonNameToLabelButton != null && this.buttonNameToLabelButton.isMouseOver(x, y)) {
      this.renderTooltip(
          poseStack,
          new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + "name_to_label.tooltip"),
          x,
          y);
    }
  }

  @Override
  protected void renderLabels(PoseStack poseStack, int x, int y) {
    // No labels
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    // Action Box
    if (this.commandDialogBox != null) {
      fill(
          poseStack,
          this.leftPos + 6,
          this.topPos + 110,
          this.rightPos - 6,
          this.commandDialogBox.y + 70,
          0xff000000);
      fill(
          poseStack,
          this.leftPos + 7,
          this.topPos + 111,
          this.rightPos - 7,
          this.commandDialogBox.y + 69,
          0xffdddddd);
    }
  }
}
