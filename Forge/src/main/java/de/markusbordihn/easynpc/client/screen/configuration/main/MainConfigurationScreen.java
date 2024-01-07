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

package de.markusbordihn.easynpc.client.screen.configuration.main;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.client.screen.components.CopyButton;
import de.markusbordihn.easynpc.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.configuration.main.MainConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class MainConfigurationScreen extends ConfigurationScreen<MainConfigurationMenu> {

  public static final int BUTTON_WIDTH = 90;
  public static final int BUTTON_HEIGHT = 18;

  private Button saveNameButton;
  private EditBox nameBox;

  // Cache
  private String formerName = "";

  public MainConfigurationScreen(
      MainConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void saveName() {
    String value = this.nameBox.getValue();
    if (value != null && !value.isBlank()) {
      log.debug("Saving name {} for {}", value, this.entity);
      NetworkMessageHandler.nameChange(this.uuid, value);
      this.formerName = value;
      this.saveNameButton.active = false;
    }
  }

  private void deleteNPC() {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed && uuid != null) {
                NetworkMessageHandler.removeNPC(uuid);
                minecraft.setScreen(null);
              } else {
                minecraft.setScreen(this);
              }
            },
            Component.translatable(Constants.TEXT_PREFIX + "removeNPC.deleteQuestion"),
            Component.translatable(
                Constants.TEXT_PREFIX + "removeNPC.deleteWarning",
                this.entity.getDisplayName().getString()),
            Component.translatable(Constants.TEXT_PREFIX + "removeNPC.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  private void respawnNPC() {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed && uuid != null) {
                NetworkMessageHandler.respawnNPC(uuid);
                minecraft.setScreen(null);
              } else {
                minecraft.setScreen(this);
              }
            },
            Component.translatable(Constants.TEXT_PREFIX + "respawnNPC.confirmQuestion"),
            Component.translatable(
                Constants.TEXT_PREFIX + "respawnNPC.confirmWarning",
                this.entity.getDisplayName().getString()),
            Component.translatable(Constants.TEXT_PREFIX + "respawnNPC.respawnButton"),
            CommonComponents.GUI_CANCEL));
  }

  private void validateName() {
    String nameValue = this.nameBox.getValue();
    this.saveNameButton.active = nameValue != null && !this.formerName.equals(nameValue);
  }

  @Override
  public void init() {
    if (this.entity == null) {
      return;
    }
    super.init();

    // Button positions
    int buttonLeftPosition = this.leftPos + 125;
    int buttonSpace = 4;
    int buttonTopPosition = this.topPos + 50;

    // Hide home button
    this.homeButton.visible = false;

    // Name Edit Box and Save Button
    this.formerName = this.entity.getName().getString();
    this.nameBox = new TextField(this.font, this.contentLeftPos + 1, this.topPos + 25, 108);
    this.nameBox.setMaxLength(32);
    this.nameBox.setValue(this.formerName);
    this.nameBox.setResponder(consumer -> this.validateName());
    this.addRenderableWidget(this.nameBox);
    this.saveNameButton =
        this.addRenderableWidget(
            new SaveButton(this.leftPos + 118, this.topPos + 24, onPress -> this.saveName()));
    this.saveNameButton.active = false;

    // Skins Button
    Button editSkinButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos,
                this.topPos + 195,
                110,
                "edit_skin",
                onPress -> {
                  SkinType skinType = this.entity.getSkinType();
                  switch (skinType) {
                    case NONE:
                      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.NONE_SKIN);
                      break;
                    case PLAYER_SKIN:
                      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.PLAYER_SKIN);
                      break;
                    case SECURE_REMOTE_URL, INSECURE_REMOTE_URL:
                      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.URL_SKIN);
                      break;
                    case CUSTOM:
                      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.CUSTOM_SKIN);
                      break;
                    default:
                      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.DEFAULT_SKIN);
                  }
                }));
    editSkinButton.active =
        this.supportsSkinConfiguration
            && (this.hasPermissions(
                    COMMON.defaultSkinConfigurationEnabled.get(),
                    COMMON.defaultSkinConfigurationAllowInCreative.get(),
                    COMMON.defaultSkinConfigurationPermissionLevel.get())
                || this.hasPermissions(
                    COMMON.playerSkinConfigurationEnabled.get(),
                    COMMON.playerSkinConfigurationAllowInCreative.get(),
                    COMMON.playerSkinConfigurationPermissionLevel.get())
                || this.hasPermissions(
                    COMMON.customSkinConfigurationEnabled.get(),
                    COMMON.customSkinConfigurationAllowInCreative.get(),
                    COMMON.customSkinConfigurationPermissionLevel.get()));

    // Import Button
    Button importButton =
        this.addRenderableWidget(
            new TextButton(
                buttonLeftPosition,
                buttonTopPosition,
                BUTTON_WIDTH,
                "import",
                onPress ->
                    NetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.DEFAULT_PRESET_IMPORT)));
    importButton.active = true;

    // Export Button
    Button exportButton =
        this.addRenderableWidget(
            new TextButton(
                buttonLeftPosition + importButton.getWidth() + buttonSpace,
                buttonTopPosition,
                BUTTON_WIDTH,
                "export",
                onPress ->
                    NetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.CUSTOM_PRESET_EXPORT)));
    exportButton.active = true;

    // Move button position down
    buttonTopPosition = buttonTopPosition + BUTTON_HEIGHT + buttonSpace;

    // Dialog Button
    Button editDialogButton =
        this.addRenderableWidget(
            new TextButton(
                buttonLeftPosition,
                buttonTopPosition,
                BUTTON_WIDTH,
                "dialog",
                onPress -> {
                  switch (this.menu.getDialogType()) {
                    case NONE:
                      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.NONE_DIALOG);
                      break;
                    case YES_NO:
                      NetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.YES_NO_DIALOG);
                      break;
                    case CUSTOM, STANDARD:
                      NetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.ADVANCED_DIALOG);
                      break;
                    case BASIC:
                    default:
                      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.BASIC_DIALOG);
                  }
                }));
    editDialogButton.active =
        this.hasPermissions(
                COMMON.basicDialogConfigurationEnabled.get(),
                COMMON.basicDialogConfigurationAllowInCreative.get(),
                COMMON.basicDialogConfigurationPermissionLevel.get())
            || this.hasPermissions(
                COMMON.yesNoDialogConfigurationEnabled.get(),
                COMMON.yesNoDialogConfigurationAllowInCreative.get(),
                COMMON.yesNoDialogConfigurationPermissionLevel.get())
            || this.hasPermissions(
                COMMON.noneDialogConfigurationEnabled.get(),
                COMMON.noneDialogConfigurationAllowInCreative.get(),
                COMMON.noneDialogConfigurationPermissionLevel.get());

    // Actions Button
    Button editActionButton =
        this.addRenderableWidget(
            new TextButton(
                editDialogButton.getX() + editDialogButton.getWidth() + buttonSpace,
                buttonTopPosition,
                BUTTON_WIDTH,
                "actions",
                onPress ->
                    NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.BASIC_ACTION)));
    editActionButton.active =
        this.hasPermissions(
                COMMON.basicActionConfigurationEnabled.get(),
                COMMON.basicActionConfigurationAllowInCreative.get(),
                COMMON.basicActionConfigurationPermissionLevel.get())
            || this.hasPermissions(
                COMMON.dialogActionConfigurationEnabled.get(),
                COMMON.dialogActionConfigurationAllowInCreative.get(),
                COMMON.dialogActionConfigurationPermissionLevel.get());

    // Move button position down
    buttonTopPosition = buttonTopPosition + BUTTON_HEIGHT + buttonSpace;

    // Equipment Button
    Button editEquipmentButton =
        this.addRenderableWidget(
            new TextButton(
                buttonLeftPosition,
                buttonTopPosition,
                BUTTON_WIDTH,
                "equipment",
                onPress ->
                    NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.EQUIPMENT)));
    editEquipmentButton.active =
        this.hasPermissions(
            COMMON.equipmentConfigurationEnabled.get(),
            COMMON.equipmentConfigurationAllowInCreative.get(),
            COMMON.equipmentConfigurationPermissionLevel.get());

    // Scaling Button
    Button editScalingButton =
        this.addRenderableWidget(
            new TextButton(
                editEquipmentButton.getX() + editEquipmentButton.getWidth() + buttonSpace,
                buttonTopPosition,
                BUTTON_WIDTH,
                "scaling",
                onPress ->
                    NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.SCALING)));
    editScalingButton.active =
        this.supportsScalingConfiguration
            && this.hasPermissions(
                COMMON.scalingConfigurationEnabled.get(),
                COMMON.scalingConfigurationAllowInCreative.get(),
                COMMON.scalingConfigurationPermissionLevel.get());

    // Move button position down
    buttonTopPosition = buttonTopPosition + BUTTON_HEIGHT + buttonSpace;

    // Pose Button
    Button editPoseButton =
        this.addRenderableWidget(
            new TextButton(
                buttonLeftPosition,
                buttonTopPosition,
                BUTTON_WIDTH,
                "pose",
                onPress -> {
                  ModelPose modelPose = entity.getModelPose();
                  switch (modelPose) {
                    case CUSTOM:
                      if (entity.hasChangedModelPosition()) {
                        NetworkMessageHandler.openConfiguration(
                            uuid, ConfigurationType.CUSTOM_POSE);
                      } else {
                        NetworkMessageHandler.openConfiguration(
                            uuid, ConfigurationType.ADVANCED_POSE);
                      }
                      break;
                    case DEFAULT:
                    default:
                      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.DEFAULT_POSE);
                  }
                }));
    editPoseButton.active =
        this.supportsPoseConfiguration
            && (this.hasPermissions(
                    COMMON.defaultPoseConfigurationEnabled.get(),
                    COMMON.defaultPoseConfigurationAllowInCreative.get(),
                    COMMON.defaultPoseConfigurationPermissionLevel.get())
                || this.hasPermissions(
                    COMMON.customPoseConfigurationEnabled.get(),
                    COMMON.customPoseConfigurationAllowInCreative.get(),
                    COMMON.customPoseConfigurationPermissionLevel.get()));

    // Position Button
    Button editPositionButton =
        this.addRenderableWidget(
            new TextButton(
                editPoseButton.getX() + editPoseButton.getWidth() + buttonSpace,
                buttonTopPosition,
                BUTTON_WIDTH,
                "position",
                onPress ->
                    NetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.DEFAULT_POSITION)));
    editPositionButton.active =
        this.hasPermissions(
            COMMON.defaultPositionConfigurationEnabled.get(),
            COMMON.defaultPositionConfigurationAllowInCreative.get(),
            COMMON.defaultPositionConfigurationPermissionLevel.get());

    // Move button position down
    buttonTopPosition = buttonTopPosition + BUTTON_HEIGHT + buttonSpace;

    // Rotation Button
    Button editRotationButton =
        this.addRenderableWidget(
            new TextButton(
                buttonLeftPosition,
                buttonTopPosition,
                BUTTON_WIDTH,
                "rotation",
                onPress ->
                    NetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.DEFAULT_ROTATION)));
    editRotationButton.active =
        this.hasPermissions(
            COMMON.defaultRotationConfigurationEnabled.get(),
            COMMON.defaultRotationConfigurationAllowInCreative.get(),
            COMMON.defaultRotationConfigurationPermissionLevel.get());

    // Trades Button
    Button editTradesButton =
        this.addRenderableWidget(
            new TextButton(
                editRotationButton.getX() + editRotationButton.getWidth() + buttonSpace,
                buttonTopPosition,
                BUTTON_WIDTH,
                "trading",
                onPress -> {
                  TradingType tradingType = this.entity.getTradingType();
                  switch (tradingType) {
                    case BASIC:
                      NetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.BASIC_TRADING);
                      break;
                    case ADVANCED:
                      NetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.ADVANCED_TRADING);
                      break;
                    case CUSTOM:
                      NetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.CUSTOM_TRADING);
                      break;
                    case NONE:
                    default:
                      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.NONE_TRADING);
                  }
                }));
    editTradesButton.active =
        this.hasPermissions(
                COMMON.basicTradingConfigurationEnabled.get(),
                COMMON.basicTradingConfigurationAllowInCreative.get(),
                COMMON.basicTradingConfigurationPermissionLevel.get())
            || this.hasPermissions(
                COMMON.advancedTradingConfigurationEnabled.get(),
                COMMON.advancedTradingConfigurationAllowInCreative.get(),
                COMMON.advancedTradingConfigurationPermissionLevel.get())
            || this.hasPermissions(
                COMMON.customTradingConfigurationEnabled.get(),
                COMMON.customTradingConfigurationAllowInCreative.get(),
                COMMON.customTradingConfigurationPermissionLevel.get());

    // Move button position down
    buttonTopPosition = buttonTopPosition + BUTTON_HEIGHT + buttonSpace;

    // Attributes Button
    Button editAttributes =
        this.addRenderableWidget(
            new TextButton(
                buttonLeftPosition,
                buttonTopPosition,
                BUTTON_WIDTH,
                "attributes",
                onPress ->
                    NetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.ABILITIES_ATTRIBUTE)));

    // Objective Button
    Button editObjectiveButton =
        this.addRenderableWidget(
            new TextButton(
                editAttributes.getX() + editAttributes.getWidth() + buttonSpace,
                buttonTopPosition,
                BUTTON_WIDTH,
                "objective",
                onPress ->
                    NetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.BASIC_OBJECTIVE)));

    // Copy UUID Button
    Button copyUUIDButton =
        this.addRenderableWidget(
            new CopyButton(
                this.contentLeftPos,
                this.bottomPos - 27,
                90,
                "copy_uuid",
                onPress -> {
                  Minecraft minecraft = Minecraft.getInstance();
                  if (minecraft != null && minecraft.keyboardHandler != null) {
                    minecraft.keyboardHandler.setClipboard(uuid.toString());
                  }
                }));
    copyUUIDButton.active = true;

    // Respawn Button
    Button respawnButton =
        this.addRenderableWidget(
            new TextButton(
                copyUUIDButton.getX() + copyUUIDButton.getWidth() + buttonSpace,
                this.bottomPos - 27,
                70,
                "respawn",
                onPress -> respawnNPC()));
    respawnButton.active = true;

    // Delete Button
    Button removeEntityButton =
        this.addRenderableWidget(
            new DeleteButton(
                this.rightPos - 70, this.bottomPos - 29, 65, onPress -> this.deleteNPC()));
    removeEntityButton.setFGColor(16733525);
    removeEntityButton.active = true;
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);
    this.xMouse = x;
    this.yMouse = y;

    // Avatar
    ScreenHelper.renderScaledEntityAvatar(
        this.leftPos + 60,
        this.topPos + 185,
        this.leftPos + 50 - this.xMouse,
        this.topPos + 90 - this.yMouse,
        this.entity);

    // Entity Type
    float scaleEntityTypeText = 0.75f;
    guiGraphics.pose().pushPose();
    guiGraphics.pose().scale(scaleEntityTypeText, scaleEntityTypeText, scaleEntityTypeText);
    Text.drawString(
        guiGraphics,
        this.font,
        entity.getType().getDescription(),
        Math.round((this.contentLeftPos + 3) / scaleEntityTypeText),
        Math.round((this.topPos + 49) / scaleEntityTypeText));

    // Make sure that entity text is always on top
    guiGraphics.pose().translate(0, 0, 100);

    // Entity Owner, if available.
    if (this.hasOwner) {
      Text.drawString(
          guiGraphics,
          this.font,
          "Owner: " + this.ownerName,
          Math.round((this.contentLeftPos + 3) / scaleEntityTypeText),
          Math.round((this.topPos + 61) / scaleEntityTypeText));
    }

    // Entity UUID, if available.
    if (this.uuid != null) {
      Text.drawString(
          guiGraphics,
          this.font,
          "UUID: " + this.uuid,
          Math.round((this.contentLeftPos + 1) / scaleEntityTypeText),
          Math.round((this.topPos + 15) / scaleEntityTypeText));
    }

    // Entity Position
    BlockPos blockPos = this.entity.getOnPos();
    Text.drawString(
        guiGraphics,
        this.font,
        "Pos: " + blockPos.getX() + ", " + blockPos.getY() + ", " + blockPos.getZ(),
        Math.round((this.contentLeftPos + 20) / scaleEntityTypeText),
        Math.round((this.topPos + 187) / scaleEntityTypeText));
    guiGraphics.pose().popPose();
  }

  @Override
  protected void renderLabels(GuiGraphics guiGraphics, int x, int y) {
    Text.drawString(
        guiGraphics, this.font, this.title, this.titleLabelX, this.titleLabelY - 1, 4210752);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    int avatarTopPos = this.topPos + 45;

    // Entity Type
    guiGraphics.fill(
        this.contentLeftPos, avatarTopPos, this.leftPos + 117, avatarTopPos + 135, 0xff000000);
    guiGraphics.fill(
        this.leftPos + 8, avatarTopPos + 1, this.leftPos + 116, avatarTopPos + 134, 0xffffffff);

    // Entity
    guiGraphics.fill(
        this.contentLeftPos, avatarTopPos + 13, this.leftPos + 117, avatarTopPos + 155, 0xff000000);
    guiGraphics.fill(
        this.leftPos + 8, avatarTopPos + 14, this.leftPos + 116, avatarTopPos + 154, 0xffaaaaaa);
  }
}
