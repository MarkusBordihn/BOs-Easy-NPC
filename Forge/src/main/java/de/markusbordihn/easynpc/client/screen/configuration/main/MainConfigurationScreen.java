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

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.ColorButton;
import de.markusbordihn.easynpc.client.screen.components.CopyButton;
import de.markusbordihn.easynpc.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderData;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.menu.configuration.main.MainConfigurationMenu;
import de.markusbordihn.easynpc.network.ServerNetworkMessageHandler;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.item.DyeColor;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class MainConfigurationScreen extends ConfigurationScreen<MainConfigurationMenu> {

  public static final int BUTTON_WIDTH = 90;
  public static final int BUTTON_HEIGHT = 18;
  private ColorButton nameColorButton;
  private Button saveNameButton;
  private EditBox nameBox;
  private int textColor = 0xFFFFFF;
  private String formerName = "";
  private int formerTextColor = 0xFFFFFF;

  public MainConfigurationScreen(
      MainConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void saveName() {
    String value = this.nameBox.getValue();
    if (value != null && !value.isBlank()) {
      if (this.nameColorButton != null) {
        textColor = this.nameColorButton.getColorValue();
      }
      log.debug("Saving name {} with color {} for {}", value, textColor, this.easyNPC);
      ServerNetworkMessageHandler.nameChange(this.uuid, value, textColor);
      this.formerName = value;
      this.formerTextColor = textColor;
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
                ServerNetworkMessageHandler.removeNPC(uuid);
                minecraft.setScreen(null);
              } else {
                minecraft.setScreen(this);
              }
            },
            new TranslatableComponent(Constants.TEXT_PREFIX + "removeNPC.deleteQuestion"),
            new TranslatableComponent(
                Constants.TEXT_PREFIX + "removeNPC.deleteWarning",
                this.easyNPC.getEntity().getDisplayName().getString()),
            new TranslatableComponent(Constants.TEXT_PREFIX + "removeNPC.deleteButton"),
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
                ServerNetworkMessageHandler.respawnNPC(uuid);
                minecraft.setScreen(null);
              } else {
                minecraft.setScreen(this);
              }
            },
            new TranslatableComponent(Constants.TEXT_PREFIX + "respawnNPC.confirmQuestion"),
            new TranslatableComponent(
                Constants.TEXT_PREFIX + "respawnNPC.confirmWarning",
                this.easyNPC.getEntity().getDisplayName().getString()),
            new TranslatableComponent(Constants.TEXT_PREFIX + "respawnNPC.respawnButton"),
            CommonComponents.GUI_CANCEL));
  }

  private void validateName() {
    String nameValue = this.nameBox.getValue();
    int textColorValue = this.nameColorButton.getColorValue();
    this.saveNameButton.active =
        nameValue != null
            && (!this.formerName.equals(nameValue) || this.formerTextColor != textColorValue);
  }

  @Override
  public void init() {
    if (this.easyNPC == null) {
      return;
    }
    super.init();

    // Button positions
    int buttonLeftPosition = this.leftPos + 125;
    int buttonSpace = 4;
    int buttonTopPosition = this.topPos + 50;

    // Hide home button
    this.homeButton.visible = false;

    // Data access
    RenderData<?> renderData = this.easyNPC.getEasyNPCRenderData();
    RenderType renderType =
        renderData != null && renderData.getRenderData() != null
            ? renderData.getRenderData().getRenderType()
            : RenderType.DEFAULT;

    // Name Edit Box and Save Button
    this.formerName = this.easyNPC.getEntity().getName().getString();
    this.nameBox = new TextField(this.font, this.contentLeftPos + 1, this.topPos + 25, 108);
    this.nameBox.setMaxLength(32);
    this.nameBox.setValue(this.formerName);
    this.nameBox.setResponder(consumer -> this.validateName());
    this.addRenderableWidget(this.nameBox);

    this.nameColorButton =
        this.addRenderableWidget(
            new ColorButton(this.leftPos + 119, this.topPos + 24, onPress -> this.validateName()));
    if (this.easyNPC.getEntity().hasCustomName()
        && this.easyNPC.getEntity().getCustomName().getStyle() != null
        && this.easyNPC.getEntity().getCustomName().getStyle().getColor() != null) {
      int styleTextColor =
          this.easyNPC.getEntity().getCustomName().getStyle().getColor().getValue();
      for (DyeColor dyeColor : DyeColor.values()) {
        if (dyeColor.getTextColor() == styleTextColor) {
          this.nameColorButton.setColor(dyeColor);
          this.formerTextColor = styleTextColor;
          break;
        }
      }
    }

    this.saveNameButton =
        this.addRenderableWidget(
            new SaveButton(this.leftPos + 139, this.topPos + 24, onPress -> this.saveName()));
    this.saveNameButton.active = false;

    // Skins Button
    SkinData<?> skinData = this.easyNPC.getEasyNPCSkinData();
    Button editSkinButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos,
                this.topPos + 195,
                110,
                "edit_skin",
                onPress -> {
                  SkinType skinType = skinData.getSkinType();
                  switch (skinType) {
                    case NONE:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.NONE_SKIN);
                      break;
                    case PLAYER_SKIN:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.PLAYER_SKIN);
                      break;
                    case SECURE_REMOTE_URL, INSECURE_REMOTE_URL:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.URL_SKIN);
                      break;
                    case CUSTOM:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.CUSTOM_SKIN);
                      break;
                    default:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.DEFAULT_SKIN);
                  }
                }));
    editSkinButton.active =
        renderType == RenderType.DEFAULT
            && this.supportsSkinConfiguration
            && (this.hasPermissions(
                    COMMON.noneSkinConfigurationEnabled.get(),
                    COMMON.noneSkinConfigurationAllowInCreative.get(),
                    COMMON.noneSkinConfigurationPermissionLevel.get())
                || this.hasPermissions(
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
                    ServerNetworkMessageHandler.openConfiguration(
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
                    ServerNetworkMessageHandler.openConfiguration(
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
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.NONE_DIALOG);
                      break;
                    case YES_NO:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.YES_NO_DIALOG);
                      break;
                    case CUSTOM, STANDARD:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.ADVANCED_DIALOG);
                      break;
                    case BASIC:
                    default:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.BASIC_DIALOG);
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
                editDialogButton.x + editDialogButton.getWidth() + buttonSpace,
                buttonTopPosition,
                BUTTON_WIDTH,
                "actions",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.BASIC_ACTION)));
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
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.EQUIPMENT)));
    editEquipmentButton.active =
        this.hasPermissions(
            COMMON.equipmentConfigurationEnabled.get(),
            COMMON.equipmentConfigurationAllowInCreative.get(),
            COMMON.equipmentConfigurationPermissionLevel.get());

    // Scaling
    ScaleData<?> scaleData = this.easyNPC.getEasyNPCScaleData();
    Button editScalingButton =
        this.addRenderableWidget(
            new TextButton(
                editEquipmentButton.x + editEquipmentButton.getWidth() + buttonSpace,
                buttonTopPosition,
                BUTTON_WIDTH,
                "scaling",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.SCALING)));
    editScalingButton.active =
        renderType == RenderType.DEFAULT
            && scaleData != null
            && this.supportsScalingConfiguration
            && this.hasPermissions(
                COMMON.scalingConfigurationEnabled.get(),
                COMMON.scalingConfigurationAllowInCreative.get(),
                COMMON.scalingConfigurationPermissionLevel.get());

    // Move button position down
    buttonTopPosition = buttonTopPosition + BUTTON_HEIGHT + buttonSpace;

    // Pose Button
    ModelData<?> modelData = this.easyNPC.getEasyNPCModelData();
    Button editPoseButton =
        this.addRenderableWidget(
            new TextButton(
                buttonLeftPosition,
                buttonTopPosition,
                BUTTON_WIDTH,
                "pose",
                onPress -> {
                  ModelPose modelPose = modelData.getModelPose();
                  switch (modelPose) {
                    case CUSTOM:
                      if (modelData.hasChangedModelPosition()) {
                        ServerNetworkMessageHandler.openConfiguration(
                            uuid, ConfigurationType.CUSTOM_POSE);
                      } else {
                        ServerNetworkMessageHandler.openConfiguration(
                            uuid, ConfigurationType.ADVANCED_POSE);
                      }
                      break;
                    case DEFAULT:
                    default:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.DEFAULT_POSE);
                  }
                }));
    editPoseButton.active =
        renderType == RenderType.DEFAULT
            && modelData != null
            && this.supportsPoseConfiguration
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
                editPoseButton.x + editPoseButton.getWidth() + buttonSpace,
                buttonTopPosition,
                BUTTON_WIDTH,
                "position",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
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
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.DEFAULT_ROTATION)));
    editRotationButton.active =
        renderType == RenderType.DEFAULT
            && modelData != null
            && this.hasPermissions(
                COMMON.defaultRotationConfigurationEnabled.get(),
                COMMON.defaultRotationConfigurationAllowInCreative.get(),
                COMMON.defaultRotationConfigurationPermissionLevel.get());

    // Trades Button
    TradingData<?> tradingData = this.easyNPC.getEasyNPCTradingData();
    Button editTradesButton =
        this.addRenderableWidget(
            new TextButton(
                editRotationButton.x + editRotationButton.getWidth() + buttonSpace,
                buttonTopPosition,
                BUTTON_WIDTH,
                "trading",
                onPress -> {
                  TradingType tradingType = tradingData.getTradingType();
                  switch (tradingType) {
                    case BASIC:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.BASIC_TRADING);
                      break;
                    case ADVANCED:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.ADVANCED_TRADING);
                      break;
                    case CUSTOM:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.CUSTOM_TRADING);
                      break;
                    case NONE:
                    default:
                      ServerNetworkMessageHandler.openConfiguration(
                          uuid, ConfigurationType.NONE_TRADING);
                  }
                }));
    editTradesButton.active =
        tradingData != null
                && this.hasPermissions(
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
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.ABILITIES_ATTRIBUTE)));
    editAttributes.active =
        this.hasPermissions(
            COMMON.abilitiesAttributeConfigurationEnabled.get(),
            COMMON.abilitiesAttributeConfigurationAllowInCreative.get(),
            COMMON.abilitiesAttributeConfigurationPermissionLevel.get());

    // Objective Button
    Button editObjectiveButton =
        this.addRenderableWidget(
            new TextButton(
                editAttributes.x + editAttributes.getWidth() + buttonSpace,
                buttonTopPosition,
                BUTTON_WIDTH,
                "objective",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.BASIC_OBJECTIVE)));
    editObjectiveButton.active =
        this.hasPermissions(
            COMMON.basicObjectiveConfigurationEnabled.get(),
            COMMON.basicObjectiveConfigurationAllowInCreative.get(),
            COMMON.basicObjectiveConfigurationPermissionLevel.get());

    // Copy UUID Button
    // Buttons and boxes
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
                copyUUIDButton.x + copyUUIDButton.getWidth() + buttonSpace,
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
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);
    this.xMouse = x;
    this.yMouse = y;

    // Avatar
    ScreenHelper.renderScaledEntityAvatar(
        this.leftPos + 60,
        this.topPos + 185,
        this.leftPos + 50 - this.xMouse,
        this.topPos + 90 - this.yMouse,
        this.easyNPC);

    // Entity Type
    float scaleEntityTypeText = 0.75f;
    poseStack.pushPose();
    poseStack.scale(scaleEntityTypeText, scaleEntityTypeText, scaleEntityTypeText);
    Text.drawString(
        poseStack,
        this.font,
        this.easyNPC.getEntity().getType().getDescription(),
        Math.round((this.contentLeftPos + 3) / scaleEntityTypeText),
        Math.round((this.topPos + 49) / scaleEntityTypeText));

    // Make sure that entity text is always on top
    poseStack.translate(0, 0, 100);

    // Entity UUID, if available.
    if (this.uuid != null) {
      Text.drawString(
          poseStack,
          this.font,
          "UUID: " + this.uuid,
          Math.round((this.contentLeftPos + 1) / scaleEntityTypeText),
          Math.round((this.topPos + 15) / scaleEntityTypeText));
    }

    // Entity Owner, if available.
    if (this.hasOwner) {
      Text.drawString(
          poseStack,
          this.font,
          "Owner: " + this.ownerName,
          Math.round((this.contentLeftPos + 3) / scaleEntityTypeText),
          Math.round((this.topPos + 61) / scaleEntityTypeText));
    }

    // Home position
    NavigationData<?> navigationData = this.easyNPC.getEasyNPCNavigationData();
    if (navigationData.hasHomePosition()) {
      BlockPos blockPos = navigationData.getHomePosition();
      Text.drawString(
          poseStack,
          this.font,
          "Home: " + blockPos.getX() + ", " + blockPos.getY() + ", " + blockPos.getZ(),
          Math.round((this.contentLeftPos + 3) / scaleEntityTypeText),
          Math.round((this.topPos + 70) / scaleEntityTypeText));
    }

    // Current position
    BlockPos blockPos = this.easyNPC.getEntity().getOnPos();
    Text.drawString(
        poseStack,
        this.font,
        "Pos: " + blockPos.getX() + ", " + blockPos.getY() + ", " + blockPos.getZ(),
        Math.round((this.contentLeftPos + 3) / scaleEntityTypeText),
        Math.round((this.topPos + 187) / scaleEntityTypeText));
    poseStack.popPose();
  }

  @Override
  protected void renderLabels(PoseStack poseStack, int x, int y) {
    Text.drawString(
        poseStack, this.font, this.title, this.titleLabelX, this.titleLabelY - 1, 4210752);
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    int avatarTopPos = this.topPos + 45;

    // Entity Type
    fill(
        poseStack,
        this.contentLeftPos,
        avatarTopPos,
        this.leftPos + 117,
        avatarTopPos + 135,
        0xff000000);
    fill(
        poseStack,
        this.leftPos + 8,
        avatarTopPos + 1,
        this.leftPos + 116,
        avatarTopPos + 134,
        0xffffffff);

    // Entity
    fill(
        poseStack,
        this.contentLeftPos,
        avatarTopPos + 13,
        this.leftPos + 117,
        avatarTopPos + 155,
        0xff000000);
    fill(
        poseStack,
        this.leftPos + 8,
        avatarTopPos + 14,
        this.leftPos + 116,
        avatarTopPos + 154,
        0xffaaaaaa);
  }
}
