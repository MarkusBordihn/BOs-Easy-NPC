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

package de.markusbordihn.easynpc.client.screen.configuration.main;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.ColorButton;
import de.markusbordihn.easynpc.client.screen.components.CopyButton;
import de.markusbordihn.easynpc.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.client.screen.components.ReloadButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.render.RenderDataSet;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import java.util.LinkedHashMap;
import java.util.Map;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.contents.TranslatableContents;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.item.DyeColor;

public class MainConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  public static final int BUTTON_HEIGHT = 18;
  public static final int BUTTON_WIDTH = 92;
  private static final Map<String, ConfigurationType> menuButtons = new LinkedHashMap<>();

  static {
    menuButtons.put("dialog", ConfigurationType.DIALOG);
    menuButtons.put("actions", ConfigurationType.BASIC_ACTION);
    menuButtons.put("equipment", ConfigurationType.EQUIPMENT);
    menuButtons.put("scaling", ConfigurationType.SCALING);
    menuButtons.put("pose", ConfigurationType.POSE);
    menuButtons.put("position", ConfigurationType.DEFAULT_POSITION);
    menuButtons.put("rotation", ConfigurationType.DEFAULT_ROTATION);
    menuButtons.put("trading", ConfigurationType.TRADING);
    menuButtons.put("attributes", ConfigurationType.ABILITIES_ATTRIBUTE);
    menuButtons.put("objective", ConfigurationType.BASIC_OBJECTIVE);
  }

  private Button copyUUIDButton;
  private String formerName = "";
  private int formerTextColor = 0xFFFFFF;
  private EditBox nameBox;
  private ColorButton nameColorButton;
  private Button saveNameButton;
  private int textColor = 0xFFFFFF;

  public MainConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.showCloseButton = true;
  }

  @Override
  public void init() {
    super.init();

    // Core Positions
    this.contentTopPos = this.topPos + 15;

    // Hide home button
    this.homeButton.visible = false;

    // Define buttons and boxes
    this.defineNameAndColorBox();
    this.defineImportExportButtons();
    this.defineUUIDButton();
    this.defineRespawnButton();
    this.defineDeleteButton();
    this.defineEditSkinButton();
    this.defineChangeModelButton();
    this.defineMenuButtons();
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    if (getEasyNPC() == null) {
      return;
    }

    // Avatar
    ScreenHelper.renderScaledEntityAvatar(
        this.leftPos + 60,
        this.contentTopPos + 160,
        this.leftPos + 50 - this.xMouse,
        this.contentTopPos + 70 - this.yMouse,
        getEasyNPC());

    // Scale entity texts
    float scaleEntityTypeText = 0.75f;
    poseStack.pushPose();
    poseStack.scale(scaleEntityTypeText, scaleEntityTypeText, scaleEntityTypeText);

    // Entity UUID.
    Text.drawString(
        poseStack,
        this.font,
        "UUID: " + getEasyNPCEntity().getUUID(),
        Math.round((this.contentLeftPos + 1) / scaleEntityTypeText),
        Math.round((this.buttonTopPos + 1) / scaleEntityTypeText));

    // Entity Type
    Text.drawString(
        poseStack,
        this.font,
        getEasyNPCEntity().getType().getDescription(),
        Math.round((this.contentLeftPos + 3) / scaleEntityTypeText),
        Math.round((this.contentTopPos + 24) / scaleEntityTypeText));

    // Make sure that entity text is always on top
    poseStack.translate(0, 0, 100);

    // Entity Owner, if available.
    OwnerData<?> ownerData = getEasyNPC().getEasyNPCOwnerData();
    if (ownerData != null && ownerData.hasOwner()) {
      Text.drawString(
          poseStack,
          this.font,
          "Owner: " + ownerData.getOwnerName(),
          Math.round((this.contentLeftPos + 3) / scaleEntityTypeText),
          Math.round((this.contentTopPos + 35) / scaleEntityTypeText));
    }

    // Home position
    NavigationData<?> navigationData = getEasyNPC().getEasyNPCNavigationData();
    if (navigationData != null && navigationData.hasHomePosition()) {
      BlockPos blockPos = navigationData.getHomePosition();
      Text.drawString(
          poseStack,
          this.font,
          "Home: " + blockPos.getX() + ", " + blockPos.getY() + ", " + blockPos.getZ(),
          Math.round((this.contentLeftPos + 3) / scaleEntityTypeText),
          Math.round((this.contentTopPos + 43) / scaleEntityTypeText));
    }

    // Current position
    BlockPos blockPos = getEasyNPCEntity().getOnPos();
    Text.drawString(
        poseStack,
        this.font,
        "Pos: " + blockPos.getX() + ", " + blockPos.getY() + ", " + blockPos.getZ(),
        Math.round((this.contentLeftPos + 3) / scaleEntityTypeText),
        Math.round((this.contentTopPos + 163) / scaleEntityTypeText));
    poseStack.popPose();
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    int avatarTopPos = this.contentTopPos + 20;

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
        avatarTopPos + 12,
        this.leftPos + 117,
        avatarTopPos + 160,
        0xff000000);
    fill(
        poseStack,
        this.leftPos + 8,
        avatarTopPos + 13,
        this.leftPos + 116,
        avatarTopPos + 160,
        0xffaaaaaa);
  }

  private void defineImportExportButtons() {
    // Import Button
    Button importButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + 134,
                this.contentTopPos,
                80,
                "import",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(
                            this.getEasyNPCUUID(), ConfigurationType.DEFAULT_PRESET_IMPORT)));
    importButton.active = true;

    // Export Button
    Button exportButton =
        this.addRenderableWidget(
            new TextButton(
                importButton.x + importButton.getWidth() + 5,
                importButton.y,
                80,
                "export",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(
                            this.getEasyNPCUUID(), ConfigurationType.CUSTOM_PRESET_EXPORT)));
    exportButton.active = true;
  }

  private void defineNameAndColorBox() {
    // Name Edit Box and Save Button
    Component nameComponent = getEasyNPCEntity().getName();
    this.formerName =
        nameComponent.getContents() instanceof TranslatableContents translatableContents
            ? translatableContents.getKey()
            : nameComponent.getString();
    this.nameBox = new TextField(this.font, this.contentLeftPos, this.contentTopPos, 70);
    this.nameBox.setMaxLength(32);
    this.nameBox.setValue(this.formerName);
    this.nameBox.setResponder(consumer -> this.validateName());
    this.addRenderableWidget(this.nameBox);

    this.nameColorButton =
        this.addRenderableWidget(
            new ColorButton(this.leftPos + 78, this.nameBox.y - 1, onPress -> this.validateName()));
    if (getEasyNPCEntity().hasCustomName()
        && getEasyNPCEntity().getCustomName().getStyle() != null
        && getEasyNPCEntity().getCustomName().getStyle().getColor() != null) {
      int styleTextColor = getEasyNPCEntity().getCustomName().getStyle().getColor().getValue();
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
            new SaveButton(this.leftPos + 97, this.nameBox.y - 1, onPress -> this.saveName()));
    this.saveNameButton.active = false;
  }

  private void defineUUIDButton() {
    this.copyUUIDButton =
        this.addRenderableWidget(
            new CopyButton(
                this.contentLeftPos,
                this.bottomPos - 27,
                90,
                "copy_uuid",
                onPress -> {
                  Minecraft minecraft = Minecraft.getInstance();
                  minecraft.keyboardHandler.setClipboard(this.getEasyNPCUUID().toString());
                }));
  }

  private void defineRespawnButton() {
    this.addRenderableWidget(
        new ReloadButton(
            copyUUIDButton.x + copyUUIDButton.getWidth() + 4,
            this.bottomPos - 27,
            80,
            16,
            "respawn",
            onPress -> respawnNPC()));
  }

  private void defineDeleteButton() {
    this.addRenderableWidget(
        new DeleteButton(this.rightPos - 70, this.bottomPos - 29, 65, onPress -> this.deleteNPC()));
  }

  protected void defineEditSkinButton() {
    SkinData<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    if (skinData == null) {
      return;
    }
    Button editSkinButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos,
                this.topPos + 186,
                110,
                14,
                "edit_skin",
                onPress -> {
                  SkinType skinType = skinData.getSkinType();
                  switch (skinType) {
                    case NONE:
                      NetworkMessageHandlerManager.getServerHandler()
                          .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.NONE_SKIN);
                      break;
                    case PLAYER_SKIN:
                      NetworkMessageHandlerManager.getServerHandler()
                          .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.PLAYER_SKIN);
                      break;
                    case SECURE_REMOTE_URL, INSECURE_REMOTE_URL:
                      NetworkMessageHandlerManager.getServerHandler()
                          .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.URL_SKIN);
                      break;
                    case CUSTOM:
                      NetworkMessageHandlerManager.getServerHandler()
                          .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.CUSTOM_SKIN);
                      break;
                    default:
                      NetworkMessageHandlerManager.getServerHandler()
                          .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.DEFAULT_SKIN);
                  }
                }));
    editSkinButton.active =
        this.getConfigurationData().supportsConfigurationType(ConfigurationType.SKIN);
  }

  protected void defineChangeModelButton() {
    RenderDataSet renderDataSet = this.getRenderDataSet();
    if (renderDataSet == null) {
      return;
    }
    Button changeModelButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos,
                this.topPos + 199,
                110,
                14,
                "change_model",
                onPress -> {
                  switch (renderDataSet.getRenderType()) {
                    case CUSTOM_ENTITY:
                      NetworkMessageHandlerManager.getServerHandler()
                          .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.CUSTOM_MODEL);
                      break;
                    default:
                      NetworkMessageHandlerManager.getServerHandler()
                          .openConfiguration(
                              this.getEasyNPCUUID(), ConfigurationType.DEFAULT_MODEL);
                      break;
                  }
                }));
    changeModelButton.active = this.getConfigurationData().supportsChangeModelConfiguration();
  }

  protected void defineMenuButtons() {
    int buttonTopPos = this.topPos + 58;
    int buttonLeftPos = this.contentLeftPos + 115;
    int buttonIndex = 0;

    for (Map.Entry<String, ConfigurationType> entry : menuButtons.entrySet()) {
      String buttonName = entry.getKey();
      ConfigurationType configurationType = entry.getValue();
      Button button =
          this.addRenderableWidget(
              new TextButton(
                  buttonLeftPos + ((buttonIndex % 2) * (BUTTON_WIDTH + 5)),
                  buttonTopPos + ((buttonIndex / 2) * (BUTTON_HEIGHT + 2)),
                  BUTTON_WIDTH,
                  buttonName,
                  onPress ->
                      NetworkMessageHandlerManager.getServerHandler()
                          .openConfiguration(this.getEasyNPCUUID(), configurationType)));
      button.active = this.getConfigurationData().supportsConfigurationType(configurationType);
      buttonIndex++;
    }
  }

  private void respawnNPC() {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed) {
                NetworkMessageHandlerManager.getServerHandler().respawnNPC(this.getEasyNPCUUID());
                minecraft.setScreen(null);
              } else {
                minecraft.setScreen(this);
              }
            },
            Component.translatable(Constants.TEXT_PREFIX + "respawnNPC.confirmQuestion"),
            Component.translatable(
                Constants.TEXT_PREFIX + "respawnNPC.confirmWarning",
                getEasyNPCEntity().getDisplayName().getString()),
            Component.translatable(Constants.TEXT_PREFIX + "respawnNPC.respawnButton"),
            CommonComponents.GUI_CANCEL));
  }

  private void deleteNPC() {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed) {
                NetworkMessageHandlerManager.getServerHandler().removeNPC(this.getEasyNPCUUID());
                minecraft.setScreen(null);
              } else {
                minecraft.setScreen(this);
              }
            },
            Component.translatable(Constants.TEXT_PREFIX + "removeNPC.deleteQuestion"),
            Component.translatable(
                Constants.TEXT_PREFIX + "removeNPC.deleteWarning",
                getEasyNPCEntity().getDisplayName().getString()),
            Component.translatable(Constants.TEXT_PREFIX + "removeNPC.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  private void validateName() {
    String nameValue = this.nameBox.getValue();
    int textColorValue = this.nameColorButton.getColorValue();
    this.saveNameButton.active =
        !this.formerName.equals(nameValue) || this.formerTextColor != textColorValue;
  }

  private void saveName() {
    String value = this.nameBox.getValue();
    if (!value.isBlank()) {
      if (this.nameColorButton != null) {
        textColor = this.nameColorButton.getColorValue();
      }
      log.debug("Saving name {} with color {} for {}", value, textColor, getEasyNPC());
      NetworkMessageHandlerManager.getServerHandler()
          .changeName(getEasyNPC().getUUID(), value, textColor);
      this.formerName = value;
      this.formerTextColor = textColor;
      this.saveNameButton.active = false;
    }
  }
}
