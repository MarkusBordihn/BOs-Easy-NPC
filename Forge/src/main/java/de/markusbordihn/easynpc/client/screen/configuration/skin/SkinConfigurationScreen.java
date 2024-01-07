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

package de.markusbordihn.easynpc.client.screen.configuration.skin;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class SkinConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  // Settings
  protected static final int SKIN_PREVIEW_WIDTH = 60;

  // Buttons
  protected Button noneSkinButton = null;
  protected Button customSkinButton = null;
  protected Button defaultSkinButton = null;
  protected Button playerSkinButton = null;
  protected Button urlSkinButton = null;

  // Skin Navigation
  protected Button skinPreviousButton = null;
  protected Button skinNextButton = null;
  protected Button skinPreviousPageButton = null;
  protected Button skinNextPageButton = null;
  protected int skinStartIndex = 0;
  protected int numOfSkins = 0;
  protected int maxSkinsPerPage = 5;

  // Cache
  protected List<Button> skinButtons = new ArrayList<>();

  public SkinConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  protected void checkSkinNavigationButtonState() {
    // Check the visible for the buttons.
    boolean skinButtonShouldBeVisible = this.numOfSkins > this.maxSkinsPerPage;
    if (this.skinPreviousButton != null) {
      this.skinPreviousButton.visible = skinButtonShouldBeVisible;
    }
    if (this.skinNextButton != null) {
      this.skinNextButton.visible = skinButtonShouldBeVisible;
    }
    if (this.skinPreviousPageButton != null) {
      this.skinPreviousPageButton.visible = skinButtonShouldBeVisible;
    }
    if (this.skinNextPageButton != null) {
      this.skinNextPageButton.visible = skinButtonShouldBeVisible;
    }

    // Enable / disable buttons depending on the current skin index.
    if (this.skinPreviousButton != null) {
      this.skinPreviousButton.active = this.skinStartIndex > 0;
    }
    if (this.skinNextButton != null) {
      this.skinNextButton.active = this.skinStartIndex + this.maxSkinsPerPage < this.numOfSkins;
    }
    if (this.skinPreviousPageButton != null) {
      this.skinPreviousPageButton.active = this.skinStartIndex - this.maxSkinsPerPage > 0;
    }
    if (this.skinNextPageButton != null) {
      this.skinNextPageButton.active =
          this.skinStartIndex + 1 + this.maxSkinsPerPage < this.numOfSkins;
    }
  }

  protected void renderSkinSelectionBackground(PoseStack poseStack) {
    fill(
        poseStack,
        this.contentLeftPos,
        this.topPos + 102,
        this.contentLeftPos + 302,
        this.topPos + 188,
        0xff000000);
    fill(
        poseStack,
        this.contentLeftPos + 1,
        this.topPos + 103,
        this.contentLeftPos + 301,
        this.topPos + 187,
        0xffaaaaaa);
  }

  @Override
  public void init() {
    super.init();

    // Skin Types
    this.noneSkinButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                44,
                "disable_skin",
                onPress ->
                    NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.NONE_SKIN)));
    this.defaultSkinButton =
        this.addRenderableWidget(
            new TextButton(
                this.noneSkinButton.x + this.noneSkinButton.getWidth(),
                this.buttonTopPos,
                64,
                "default",
                onPress ->
                    NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.DEFAULT_SKIN)));
    this.playerSkinButton =
        this.addRenderableWidget(
            new TextButton(
                this.defaultSkinButton.x + this.defaultSkinButton.getWidth(),
                this.buttonTopPos,
                62,
                "player_skin",
                onPress ->
                    NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.PLAYER_SKIN)));
    this.urlSkinButton =
        this.addRenderableWidget(
            new TextButton(
                supportsPlayerSkinConfiguration
                    ? this.playerSkinButton.x + this.playerSkinButton.getWidth()
                    : this.defaultSkinButton.x + this.defaultSkinButton.getWidth(),
                this.buttonTopPos,
                40,
                "url_skin",
                onPress ->
                    NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.URL_SKIN)));
    this.customSkinButton =
        this.addRenderableWidget(
            new TextButton(
                this.urlSkinButton.x + this.urlSkinButton.getWidth(),
                this.buttonTopPos,
                80,
                "custom",
                onPress ->
                    NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.CUSTOM_SKIN)));

    // Default button stats
    this.noneSkinButton.active =
        this.supportsSkinConfiguration
            && this.supportsNoneSkinConfiguration
            && this.hasPermissions(
                COMMON.noneSkinConfigurationEnabled.get(),
                COMMON.noneSkinConfigurationAllowInCreative.get(),
                COMMON.noneSkinConfigurationPermissionLevel.get());
    this.customSkinButton.active =
        this.supportsSkinConfiguration
            && this.supportsCustomSkinConfiguration
            && this.hasPermissions(
                COMMON.customSkinConfigurationEnabled.get(),
                COMMON.customSkinConfigurationAllowInCreative.get(),
                COMMON.customSkinConfigurationPermissionLevel.get());
    this.defaultSkinButton.active =
        this.supportsSkinConfiguration
            && this.supportsDefaultSkinConfiguration
            && this.hasPermissions(
                COMMON.defaultSkinConfigurationEnabled.get(),
                COMMON.defaultSkinConfigurationAllowInCreative.get(),
                COMMON.defaultSkinConfigurationPermissionLevel.get());
    this.playerSkinButton.active =
        this.supportsSkinConfiguration
            && this.supportsPlayerSkinConfiguration
            && this.hasPermissions(
                COMMON.playerSkinConfigurationEnabled.get(),
                COMMON.playerSkinConfigurationAllowInCreative.get(),
                COMMON.playerSkinConfigurationPermissionLevel.get());
    this.urlSkinButton.active =
        this.supportsSkinConfiguration
            && this.supportsUrlSkinConfiguration
            && this.hasPermissions(
                COMMON.urlSkinConfigurationEnabled.get(),
                COMMON.urlSkinConfigurationAllowInCreative.get(),
                COMMON.urlSkinConfigurationPermissionLevel.get());

    // Default visibility
    this.playerSkinButton.visible = supportsPlayerSkinConfiguration;
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);
    this.renderSkinSelectionBackground(poseStack);
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    // Make sure we pass the mouse click to the dynamically added buttons, if any.
    if (!skinButtons.isEmpty()) {
      for (Button skinButton : skinButtons) {
        skinButton.mouseClicked(mouseX, mouseY, button);
      }
    }
    return super.mouseClicked(mouseX, mouseY, button);
  }
}
