/**
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

import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.configuration.main.MainConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;

@OnlyIn(Dist.CLIENT)
public class MainConfigurationScreen extends ConfigurationScreen<MainConfigurationMenu> {

  // Buttons and boxes
  protected Button editActionButton = null;
  protected Button editAttributes = null;
  protected Button editDialogButton = null;
  protected Button editEquipmentButton = null;
  protected Button editPoseButton = null;
  protected Button editPositionButton = null;
  protected Button editRotationButton = null;
  protected Button editScalingButton = null;
  protected Button editSkinButton = null;
  protected Button editTradesButton = null;
  protected Button exportButton = null;
  protected Button importButton = null;
  protected Button removeEntityButton = null;
  protected Button saveNameButton = null;
  private EditBox nameBox;

  // Cache
  private String formerName = "";

  public MainConfigurationScreen(MainConfigurationMenu menu, Inventory inventory,
      Component component) {
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

  public void deleteNPC() {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(new ConfirmScreen(confirmed -> {
      if (confirmed && uuid != null) {
        NetworkMessageHandler.removeNPC(uuid);
        minecraft.setScreen((Screen) null);
      } else {
        minecraft.setScreen(this);
      }
    }, Component.translatable(Constants.TEXT_PREFIX + "removeNPC.deleteQuestion"),
        Component.translatable(Constants.TEXT_PREFIX + "removeNPC.deleteWarning",
            this.entity.getDisplayName().getString()),
        Component.translatable(Constants.TEXT_PREFIX + "removeNPC.deleteButton"),
        CommonComponents.GUI_CANCEL));
  }

  private void validateName() {
    String nameValue = this.nameBox.getValue();
    this.saveNameButton.active = nameValue != null && !this.formerName.equals(nameValue);
  }

  private Button menuButton(int left, int top, String label, Button.OnPress onPress) {
    return menuButton(left, top, 88, label, onPress);
  }

  @Override
  public void init() {
    if (this.entity == null) {
      return;
    }
    super.init();

    // Button positions
    int buttonLeftPosition = this.leftPos + 110;
    int buttonSpace = 4;
    int buttonTopPosition = this.topPos + 43;

    // Hide home button
    this.homeButton.visible = false;

    // Name Edit Box and Save Button
    this.formerName = this.entity.getName().getString();
    this.nameBox = new EditBox(this.font, this.contentLeftPos + 1, this.topPos + 20, 190, 18,
        Component.translatable("Name"));
    this.nameBox.setMaxLength(32);
    this.nameBox.setValue(this.formerName);
    this.nameBox.setResponder(consumer -> this.validateName());
    this.addRenderableWidget(this.nameBox);

    this.saveNameButton = this.addRenderableWidget(
        menuButton(this.leftPos + 202, this.topPos + 19, "save_name", onPress -> {
          this.saveName();
        }));
    this.saveNameButton.active = false;

    // Skins Button
    this.editSkinButton = this.addRenderableWidget(
        menuButton(this.contentLeftPos, this.topPos + 194, 100, "skin", onPress -> {
          SkinType skinType = this.entity.getSkinType();
          switch (skinType) {
            case PLAYER_SKIN:
            case SECURE_REMOTE_URL:
            case INSECURE_REMOTE_URL:
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.PLAYER_SKIN);
              break;
            case CUSTOM:
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.CUSTOM_SKIN);
              break;
            default:
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.DEFAULT_SKIN);
          }
        }));
    this.editSkinButton.active = this.hasPermissions(COMMON.defaultSkinConfigurationEnabled.get(),
        COMMON.defaultSkinConfigurationAllowInCreative.get(),
        COMMON.defaultSkinConfigurationPermissionLevel.get())
        || this.hasPermissions(COMMON.playerSkinConfigurationEnabled.get(),
            COMMON.playerSkinConfigurationAllowInCreative.get(),
            COMMON.playerSkinConfigurationPermissionLevel.get())
        || this.hasPermissions(COMMON.customSkinConfigurationEnabled.get(),
            COMMON.customSkinConfigurationAllowInCreative.get(),
            COMMON.customSkinConfigurationPermissionLevel.get());

    // Import Button
    this.importButton = this.addRenderableWidget(
        menuButton(buttonLeftPosition, buttonTopPosition, "import", onPress -> {
          NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.DEFAULT_PRESET_IMPORT);
        }));

    // Export Button
    this.exportButton = this.addRenderableWidget(
        menuButton(buttonLeftPosition + this.importButton.getWidth() + buttonSpace,
            buttonTopPosition, "export", onPress -> {
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.CUSTOM_PRESET_EXPORT);
            }));

    // Move button position down
    buttonTopPosition = buttonTopPosition + 20 + buttonSpace;

    // Dialog Button
    this.editDialogButton = this.addRenderableWidget(
        menuButton(buttonLeftPosition, buttonTopPosition, "dialog", onPress -> {
          DialogType dialogType = this.entity.getDialogType();
          switch (dialogType) {
            case NONE:
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.NONE_DIALOG);
              break;
            case BASIC:
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.BASIC_DIALOG);
              break;
            case YES_NO:
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.YES_NO_DIALOG);
              break;
            default:
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.BASIC_DIALOG);
          }
        }));
    this.editDialogButton.active = this.hasPermissions(COMMON.basicDialogConfigurationEnabled.get(),
        COMMON.basicDialogConfigurationAllowInCreative.get(),
        COMMON.basicDialogConfigurationPermissionLevel.get())
        || this.hasPermissions(COMMON.yesNoDialogConfigurationEnabled.get(),
            COMMON.yesNoDialogConfigurationAllowInCreative.get(),
            COMMON.yesNoDialogConfigurationPermissionLevel.get())
        || this.hasPermissions(COMMON.noneDialogConfigurationEnabled.get(),
            COMMON.noneDialogConfigurationAllowInCreative.get(),
            COMMON.noneDialogConfigurationPermissionLevel.get());

    // Actions Button
    this.editActionButton = this.addRenderableWidget(
        menuButton(this.editDialogButton.getX() + this.editDialogButton.getWidth() + buttonSpace,
            buttonTopPosition, "actions", onPress -> {
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.BASIC_ACTION);
            }));
    this.editActionButton.active = this.hasPermissions(COMMON.basicActionConfigurationEnabled.get(),
        COMMON.basicActionConfigurationAllowInCreative.get(),
        COMMON.basicActionConfigurationPermissionLevel.get())
        || this.hasPermissions(COMMON.dialogActionConfigurationEnabled.get(),
            COMMON.dialogActionConfigurationAllowInCreative.get(),
            COMMON.dialogActionConfigurationPermissionLevel.get());

    // Move button position down
    buttonTopPosition = buttonTopPosition + 20 + buttonSpace;

    // Equipment Button
    this.editEquipmentButton = this.addRenderableWidget(
        menuButton(buttonLeftPosition, buttonTopPosition, "equipment", onPress -> {
          NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.EQUIPMENT);
        }));
    this.editEquipmentButton.active =
        this.hasPermissions(COMMON.equipmentConfigurationEnabled.get(),
            COMMON.equipmentConfigurationAllowInCreative.get(),
            COMMON.equipmentConfigurationPermissionLevel.get());

    // Scaling Button
    this.editScalingButton = this.addRenderableWidget(menuButton(
        this.editEquipmentButton.getX() + this.editEquipmentButton.getWidth() + buttonSpace,
        buttonTopPosition, "scaling", onPress -> {
          NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.SCALING);
        }));
    this.editScalingButton.active = this.hasPermissions(COMMON.scalingConfigurationEnabled.get(),
        COMMON.scalingConfigurationAllowInCreative.get(),
        COMMON.scalingConfigurationPermissionLevel.get());

    // Move button position down
    buttonTopPosition = buttonTopPosition + 20 + buttonSpace;

    // Pose Button
    this.editPoseButton = this
        .addRenderableWidget(menuButton(buttonLeftPosition, buttonTopPosition, "pose", onPress -> {
          ModelPose modelPose = entity.getModelPose();
          switch (modelPose) {
            case CUSTOM:
              if (!entity.getModelHeadPosition().isZero() || !entity.getModelBodyPosition().isZero()
                  || !entity.getModelLeftArmPosition().isZero()
                  || !entity.getModelRightArmPosition().isZero()
                  || !entity.getModelLeftLegPosition().isZero()
                  || !entity.getModelRightLegPosition().isZero()) {
                NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.CUSTOM_POSE);
              } else {
                NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.ADVANCED_POSE);
              }
              break;
            case DEFAULT:
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.DEFAULT_POSE);
              break;
            default:
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.DEFAULT_POSE);
          }
        }));
    this.editPoseButton.active = this.hasPermissions(COMMON.defaultPoseConfigurationEnabled.get(),
        COMMON.defaultPoseConfigurationAllowInCreative.get(),
        COMMON.defaultPoseConfigurationPermissionLevel.get())
        || this.hasPermissions(COMMON.customPoseConfigurationEnabled.get(),
            COMMON.customPoseConfigurationAllowInCreative.get(),
            COMMON.customPoseConfigurationPermissionLevel.get());

    // Position Button
    this.editPositionButton = this.addRenderableWidget(
        menuButton(this.editPoseButton.getX() + this.editPoseButton.getWidth() + buttonSpace,
            buttonTopPosition, "position", onPress -> {
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.DEFAULT_POSITION);
              this.closeScreen();
            }));
    this.editPositionButton.active =
        this.hasPermissions(COMMON.defaultPositionConfigurationEnabled.get(),
            COMMON.defaultPositionConfigurationAllowInCreative.get(),
            COMMON.defaultPositionConfigurationPermissionLevel.get());

    // Move button position down
    buttonTopPosition = buttonTopPosition + 20 + buttonSpace;

    // Rotation Button
    this.editRotationButton = this.addRenderableWidget(
        menuButton(buttonLeftPosition, buttonTopPosition, "rotation", onPress -> {
          NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.DEFAULT_ROTATION);
        }));
    this.editRotationButton.active =
        this.hasPermissions(COMMON.defaultRotationConfigurationEnabled.get(),
            COMMON.defaultRotationConfigurationAllowInCreative.get(),
            COMMON.defaultRotationConfigurationPermissionLevel.get());

    // Trades Button
    this.editTradesButton = this.addRenderableWidget(
        menuButton(this.editRotationButton.getX() + this.editRotationButton.getWidth() + buttonSpace,
            buttonTopPosition, "trading", onPress -> {
              TradingType tradingType = this.entity.getTradingType();
              switch (tradingType) {
                case NONE:
                  NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.NONE_TRADING);
                  break;
                case BASIC:
                  NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.BASIC_TRADING);
                  break;
                case ADVANCED:
                  NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.ADVANCED_TRADING);
                  break;
                case CUSTOM:
                  NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.CUSTOM_TRADING);
                  break;
                default:
                  NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.NONE_TRADING);
              }
            }));

    // Move button position down
    buttonTopPosition = buttonTopPosition + 20 + buttonSpace;

    // Attributes Button
    this.editAttributes = this.addRenderableWidget(
        menuButton(buttonLeftPosition, buttonTopPosition, "attributes", onPress -> {
          NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.DEFAULT_ATTRIBUTES);
        }));
    this.editAttributes.active = false;

    // Delete Button
    this.removeEntityButton = this.addRenderableWidget(
        menuButton(this.rightPos - 60, this.bottomPos - 30, 50, "delete", onPress -> {
          this.deleteNPC();
        }));
    this.removeEntityButton.setFGColor(16733525);
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);
    this.xMouse = x;
    this.yMouse = y;

    // Avatar
    ScreenHelper.renderScaledEntityAvatar(this.leftPos + 55, this.topPos + 185, 55,
        this.leftPos + 50 - this.xMouse, this.topPos + 90 - this.yMouse, this.entity);

    // Entity Type
    float scaleEntityTypeText = 0.8f;
    poseStack.pushPose();
    poseStack.scale(scaleEntityTypeText, scaleEntityTypeText, scaleEntityTypeText);
    this.font.draw(poseStack, entity.getType().getDescription(),
        (this.contentLeftPos + 3f) / scaleEntityTypeText, (this.topPos + 48f) / scaleEntityTypeText,
        4210752);
    poseStack.popPose();

    // Owner
    if (this.entity.hasOwner()) {
      this.font.draw(poseStack, "Owner: " + this.entity.getOwnerName(), this.contentLeftPos + 2f,
          this.topPos + 220f, 4210752);
    }
  }

  @Override
  protected void renderLabels(PoseStack poseStack, int x, int y) {
    this.font.draw(poseStack, this.title, this.titleLabelX, this.titleLabelY, 4210752);
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    int avatarTopPos = this.topPos + 43;

    // Entity Type
    fill(poseStack, this.contentLeftPos, avatarTopPos, this.leftPos + 107, avatarTopPos + 137,
        0xff000000);
    fill(poseStack, this.leftPos + 8, avatarTopPos + 1, this.leftPos + 106, avatarTopPos + 136,
        0xffffffff);

    // Entity
    fill(poseStack, this.contentLeftPos, avatarTopPos + 15, this.leftPos + 107, avatarTopPos + 152,
        0xff000000);
    fill(poseStack, this.leftPos + 8, avatarTopPos + 16, this.leftPos + 106, avatarTopPos + 151,
        0xffaaaaaa);
  }

}
