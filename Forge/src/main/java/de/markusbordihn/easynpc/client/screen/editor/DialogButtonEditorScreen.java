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

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
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
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.data.dialog.DialogData;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.editor.DialogButtonEditorMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import java.util.HashSet;
import java.util.UUID;
import javax.annotation.Nonnull;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@OnlyIn(Dist.CLIENT)
public class DialogButtonEditorScreen extends AbstractContainerScreen<DialogButtonEditorMenu> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // General
  protected final ClientLevel clientLevel;
  protected final LocalPlayer localPlayer;
  protected final Minecraft minecraftInstance;

  // Data access
  protected final DialogDataSet dialogDataSet;
  protected final DialogData dialogData;
  protected final DialogButtonData dialogButtonData;
  protected final EasyNPCEntity entity;
  protected final UUID dialogId;
  protected final UUID dialogButtonId;
  protected final UUID uuid;
  protected final ConfigurationType formerConfigurationType;

  // Navigation
  protected Button homeButton;
  protected Button dialogButton;
  protected Button dialogButtonButton;

  // Buttons
  protected Button closeButton;
  protected Button saveButton;
  protected Button cancelButton;
  protected Button deleteButton;

  // Edit Boxes
  protected TextField buttonNameBox;
  protected Button buttonNameToLabelButton;
  protected TextField buttonLabelBox;
  protected Checkbox buttonLabelCheckbox;

  // Open Named Dialog
  protected TextField openNamedDialogBox;
  protected Checkbox openTradingScreenCheckbox;

  // Command Dialog
  protected TextField commandDialogBox;
  protected Checkbox commandDialogExecuteAsUserCheckbox;
  protected Checkbox commandDialogDebugCheckbox;

  // Internal
  protected int bottomPos;
  protected float xMouse;
  protected float yMouse;
  protected int rightPos;

  // Cache
  private String buttonNameValue = "";
  private String buttonLabelValue = "";
  private String openNamedDialogValue = "";
  private String commandDialogValue = "";
  private boolean openTradingScreenValue = false;
  private boolean commandDialogExecuteAsUserValue = false;
  private boolean commandDialogDebugValue = false;

  @OnlyIn(Dist.CLIENT)
  public DialogButtonEditorScreen(
      DialogButtonEditorMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);

    // Data access
    this.uuid = menu.getUUID();
    this.entity = menu.getEntity();
    this.dialogDataSet = menu.getDialogDataSet();
    this.dialogData = this.dialogDataSet.getDialog(menu.getDialogId());
    this.dialogButtonData = menu.getDialogButtonData();
    this.dialogId = menu.getDialogId();
    this.dialogButtonId = menu.getDialogButtonId();
    this.formerConfigurationType = menu.getFormerConfigurationType();

    // General environment Data
    this.minecraftInstance = Minecraft.getInstance();
    this.localPlayer = this.minecraftInstance.player;
    this.clientLevel = this.minecraftInstance.level;
  }

  private void openPreviousScreen() {
    if (this.formerConfigurationType == ConfigurationType.DIALOG_EDITOR) {
      NetworkMessageHandler.openDialogEditor(uuid, this.dialogId, this.formerConfigurationType);
    } else if (this.formerConfigurationType == ConfigurationType.ADVANCED_DIALOG) {
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

  private void deleteDialogButton() {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed && uuid != null) {
                NetworkMessageHandler.removeDialogButton(uuid, dialogId, dialogButtonId);
                this.openPreviousScreen();
              } else {
                minecraft.setScreen(this);
              }
            },
            Component.translatable(Constants.TEXT_PREFIX + "removeDialogButton.deleteQuestion"),
            Component.translatable(
                Constants.TEXT_PREFIX + "removeDialogButton.deleteWarning",
                this.dialogButtonData.getName()),
            Component.translatable(Constants.TEXT_PREFIX + "removeDialogButton.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  protected void renderEditLabels(PoseStack poseStack) {

    Text.drawConfigString(
        poseStack, this.font, "button.name", leftPos + 12, this.buttonNameBox.getY() + 4);

    Text.drawConfigString(
        poseStack, this.font, "label_id", leftPos + 12, this.buttonLabelBox.getY() + 4);

    Text.drawConfigString(
        poseStack,
        this.font,
        "dialog_actions_on_click",
        leftPos + 12,
        this.openNamedDialogBox.getY() - 16,
        Constants.FONT_COLOR_BLACK);

    Text.drawConfigString(
        poseStack,
        this.font,
        "open_named_dialog",
        leftPos + 12,
        this.openNamedDialogBox.getY() + 4);

    Text.drawString(
        poseStack, this.font, "Actions", leftPos + 12, this.commandDialogBox.getY() - 10);
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

    // Dialog Button Data
    ActionData openNamedDialogAction =
        this.dialogButtonData.getActionData(ActionType.OPEN_NAMED_DIALOG);
    ActionData openTradingScreenAction =
        this.dialogButtonData.getActionData(ActionType.OPEN_TRADING_SCREEN);
    ActionData commandAction = this.dialogButtonData.getActionData(ActionType.COMMAND);

    // Close Button
    this.closeButton =
        this.addRenderableWidget(
            new CloseButton(this.rightPos - 15, this.topPos + 4, onPress -> closeScreen()));

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
                onPress -> this.openPreviousScreen()));

    // Dialog Button Button
    this.dialogButtonButton =
        this.addRenderableWidget(
            new DialogButtonButton(
                this.dialogButton.getX() + this.dialogButton.getWidth(),
                this.topPos + 7,
                140,
                this.dialogButtonData.getName(21),
                onPress -> {}));
    this.dialogButtonButton.active = false;

    // Button Name
    this.buttonNameValue = this.dialogButtonData.getName();
    this.buttonNameBox = new TextField(this.font, this.leftPos + 100, this.topPos + 30, 150);
    this.buttonNameBox.setMaxLength(64);
    this.buttonNameBox.setValue(this.buttonNameValue);
    this.addRenderableWidget(this.buttonNameBox);

    // Convert Button Name to Button Label
    this.buttonNameToLabelButton =
        this.addRenderableWidget(
            new SpriteButton(
                this.buttonNameBox.getX() + this.buttonNameBox.getWidth() + 1,
                this.buttonNameBox.getY() - 1,
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
    this.buttonLabelValue = this.dialogButtonData.getLabel();
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
                this.buttonLabelBox.getY() + 1,
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
                this.openNamedDialogBox.getY() + 1,
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
                commandDialogBox.getY() + 18,
                "execute_as_player",
                commandDialogExecuteAsUserValue));

    this.commandDialogDebugValue = commandAction != null && commandAction.isDebugEnabled();
    this.commandDialogDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.leftPos + 10 + 215,
                commandDialogBox.getY() + 18,
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

                  // Basic dialog button data
                  this.dialogButtonData.setName(this.buttonNameBox.getValue());
                  this.dialogButtonData.setLabel(this.buttonLabelBox.getValue());

                  // Action data set
                  HashSet<ActionData> actionDataSet = new HashSet<>();

                  // Open named dialog or trading screen
                  if (this.openTradingScreenCheckbox.selected()) {
                    ActionData openTradingScreenActionData =
                        new ActionData(ActionType.OPEN_TRADING_SCREEN);
                    actionDataSet.add(openTradingScreenActionData);
                  } else if (!this.openNamedDialogBox.getValue().isEmpty()) {
                    ActionData openNamedDialogActionData =
                        new ActionData(
                            ActionType.OPEN_NAMED_DIALOG, this.openNamedDialogBox.getValue());
                    actionDataSet.add(openNamedDialogActionData);
                  }

                  // Other action commands
                  ActionData commandActionData =
                      new ActionData(
                          ActionType.COMMAND,
                          this.commandDialogBox.getValue(),
                          this.commandDialogExecuteAsUserCheckbox.selected(),
                          this.commandDialogDebugCheckbox.selected());
                  actionDataSet.add(commandActionData);

                  // Save dialog button action data.
                  this.dialogButtonData.setActionData(actionDataSet);
                  NetworkMessageHandler.saveDialogButton(
                      uuid, this.dialogId, this.dialogButtonId, this.dialogButtonData);

                  // Return back to the simple yes and no dialog editor or the full dialog editor.
                  this.openPreviousScreen();
                }));

    // Delete Button
    this.deleteButton =
        this.addRenderableWidget(
            new DeleteButton(
                this.saveButton.getX() + this.saveButton.getWidth() + 10,
                this.bottomPos - 35,
                85,
                onPress -> this.deleteDialogButton()));

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
  public void render(@Nonnull PoseStack poseStack, int x, int y, float partialTicks) {
    this.renderBackground(poseStack);
    super.render(poseStack, x, y, partialTicks);
    this.renderEditLabels(poseStack);
    this.xMouse = x;
    this.yMouse = y;

    // Render Tooltips
    if (this.buttonNameToLabelButton.isMouseOver(x, y)) {
      this.renderTooltip(
          poseStack,
          Component.translatable(Constants.TEXT_CONFIG_PREFIX + "name_to_label.tooltip"),
          x,
          y);
    }
  }

  @Override
  protected void renderLabels(@Nonnull PoseStack poseStack, int x, int y) {
    // No labels
  }

  @Override
  protected void renderBg(
      @Nonnull PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DEMO_BACKGROUND);

    // Main screen: top left
    this.blit(poseStack, leftPos, topPos, 0, 0, 210, 160);

    // Main screen: top right
    this.blit(poseStack, leftPos + 203, topPos, 132, 0, 120, 160);

    // Main screen: bottom left
    this.blit(poseStack, leftPos, topPos + 77, 0, 5, 210, 170);

    // Main screen: bottom right
    this.blit(poseStack, leftPos + 203, topPos + 77, 132, 5, 120, 170);

    // Other actions
    fill(
        poseStack,
        this.leftPos + 6,
        this.topPos + 110,
        this.rightPos - 6,
        this.commandDialogBox.getY() + 70,
        0xff000000);
    fill(
        poseStack,
        this.leftPos + 7,
        this.topPos + 111,
        this.rightPos - 7,
        this.commandDialogBox.getY() + 69,
        0xffdddddd);
  }

  @Override
  public boolean keyPressed(int keyCode, int unused1, int unused2) {
    if (keyCode != 257 && keyCode != 335 && keyCode != 69 && keyCode != 73) {
      return super.keyPressed(keyCode, unused1, unused2);
    }
    return keyCode == 257 || keyCode == 335 || keyCode == 73;
  }
}
