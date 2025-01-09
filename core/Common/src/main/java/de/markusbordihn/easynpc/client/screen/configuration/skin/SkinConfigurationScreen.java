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

import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.client.texture.TextureManager;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class SkinConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  protected static final int SKIN_PREVIEW_WIDTH = 60;
  protected static int nextTextureSkinLocationChange =
      (int) java.time.Instant.now().getEpochSecond();
  protected static int nextSkinReload = (int) java.time.Instant.now().getEpochSecond();
  protected Button noneSkinButton = null;
  protected Button customSkinButton = null;
  protected Button defaultSkinButton = null;
  protected Button playerSkinButton = null;
  protected Button urlSkinButton = null;
  protected Button skinPreviousButton = null;
  protected Button skinNextButton = null;
  protected Button skinPreviousPageButton = null;
  protected Button skinNextPageButton = null;
  protected int skinStartIndex = 0;
  protected int numOfSkins = 0;
  protected int maxSkinsPerPage = 5;
  protected int lastNumOfSkins = 0;
  protected List<Button> skinButtons = new ArrayList<>();

  public SkinConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  protected void checkSkinNavigationButtonState() {
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

  protected void renderSkinSelectionBackground(GuiGraphics guiGraphics) {
    guiGraphics.fill(
        this.contentLeftPos,
        this.contentTopPos + 104,
        this.contentLeftPos + 302,
        this.contentTopPos + 190,
        0xff000000);
    guiGraphics.fill(
        this.contentLeftPos + 1,
        this.contentTopPos + 105,
        this.contentLeftPos + 301,
        this.contentTopPos + 189,
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
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.NONE_SKIN)));
    this.defaultSkinButton =
        this.addRenderableWidget(
            new TextButton(
                this.noneSkinButton.getX() + this.noneSkinButton.getWidth(),
                this.buttonTopPos,
                64,
                "default",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.DEFAULT_SKIN)));
    this.playerSkinButton =
        this.addRenderableWidget(
            new TextButton(
                this.defaultSkinButton.getX() + this.defaultSkinButton.getWidth(),
                this.buttonTopPos,
                62,
                "player_skin",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.PLAYER_SKIN)));
    this.urlSkinButton =
        this.addRenderableWidget(
            new TextButton(
                this.playerSkinButton.getX() + this.playerSkinButton.getWidth(),
                this.buttonTopPos,
                40,
                "url_skin",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.URL_SKIN)));
    this.customSkinButton =
        this.addRenderableWidget(
            new TextButton(
                this.urlSkinButton.getX() + this.urlSkinButton.getWidth(),
                this.buttonTopPos,
                80,
                "custom",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.CUSTOM_SKIN)));

    // Clear former error messages, if any.
    TextureManager.clearLastErrorMessage();

    // Verify access rights and disable buttons if necessary.
    this.checkAccess();

    // Check if we need to hide the player skin button.
    ConfigurationData<?> configurationData = this.getEasyNPC().getEasyNPCConfigurationData();
    if (!configurationData.supportsPlayerSkinConfiguration()) {
      this.urlSkinButton.setX(this.defaultSkinButton.getX() + this.defaultSkinButton.getWidth());
      this.customSkinButton.setX(this.urlSkinButton.getX() + this.urlSkinButton.getWidth());
      this.playerSkinButton.visible = false;
    }
  }

  private void checkAccess() {
    ConfigurationData<?> configurationData = this.getEasyNPC().getEasyNPCConfigurationData();
    this.customSkinButton.active =
        this.customSkinButton.active
            && configurationData.supportsSkinConfiguration()
            && configurationData.supportsCustomSkinConfiguration();
    this.defaultSkinButton.active =
        this.defaultSkinButton.active
            && configurationData.supportsSkinConfiguration()
            && configurationData.supportsDefaultSkinConfiguration();
    this.noneSkinButton.active =
        this.noneSkinButton.active
            && configurationData.supportsSkinConfiguration()
            && configurationData.supportsNoneSkinConfiguration();
    this.playerSkinButton.active =
        this.playerSkinButton.active
            && configurationData.supportsSkinConfiguration()
            && configurationData.supportsPlayerSkinConfiguration();
    this.urlSkinButton.active =
        this.urlSkinButton.active
            && configurationData.supportsSkinConfiguration()
            && configurationData.supportsUrlSkinConfiguration();
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);
    this.renderSkinSelectionBackground(guiGraphics);
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

  protected void defineSkinNavigationButtons() {
    this.defineSkinNavigationButtons(
        this.contentTopPos + 189, this.contentLeftPos, this.rightPos - 29);
  }

  protected void defineSkinNavigationButtons(
      int skinButtonTop, int skinButtonLeft, int skinButtonRight) {

    this.skinPreviousPageButton =
        this.addRenderableWidget(
            new TextButton(
                skinButtonLeft,
                skinButtonTop,
                20,
                "<<",
                onPress -> {
                  skinStartIndex = Math.max(this.skinStartIndex - maxSkinsPerPage, 0);
                  checkSkinNavigationButtonState();
                }));
    this.skinPreviousButton =
        this.addRenderableWidget(
            new TextButton(
                skinButtonLeft + 20,
                skinButtonTop,
                20,
                "<",
                onPress -> {
                  if (this.skinStartIndex > 0) {
                    skinStartIndex--;
                  }
                  checkSkinNavigationButtonState();
                }));
    this.skinNextPageButton =
        this.addRenderableWidget(
            new TextButton(
                skinButtonRight,
                skinButtonTop,
                20,
                ">>",
                onPress -> {
                  if (this.skinStartIndex >= 0
                      && this.skinStartIndex + this.maxSkinsPerPage < this.numOfSkins) {
                    this.skinStartIndex = this.skinStartIndex + this.maxSkinsPerPage;
                  } else if (this.numOfSkins > this.maxSkinsPerPage) {
                    this.skinStartIndex = this.numOfSkins - this.maxSkinsPerPage;
                  } else {
                    this.skinStartIndex = this.numOfSkins;
                  }
                  checkSkinNavigationButtonState();
                }));
    this.skinNextButton =
        this.addRenderableWidget(
            new TextButton(
                skinButtonRight - 20,
                skinButtonTop,
                20,
                ">",
                onPress -> {
                  if (this.skinStartIndex >= 0
                      && this.skinStartIndex < this.numOfSkins - this.maxSkinsPerPage) {
                    skinStartIndex++;
                  }
                  checkSkinNavigationButtonState();
                }));
    checkSkinNavigationButtonState();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Make sure we pass the mouse movements to the dynamically added buttons, if any.
    if (!skinButtons.isEmpty()) {
      for (Button skinButton : skinButtons) {
        skinButton.render(guiGraphics, x, y, partialTicks);
      }
    }
  }
}
