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

package de.markusbordihn.easynpc.configui.client.screen.configuration.skin;

import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.ReloadButton;
import de.markusbordihn.easynpc.client.screen.components.SearchField;
import de.markusbordihn.easynpc.client.screen.components.SkinSelectionButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.render.EntityRenderOverrides;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.io.CustomSkinDataFiles;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Set;
import java.util.UUID;
import net.minecraft.Util;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class CustomSkinConfigurationScreen<T extends ConfigurationMenu>
    extends SkinConfigurationScreen<T> {

  private static final float SKIN_NAME_SCALING = 0.7f;
  private static final int ADD_SKIN_RELOAD_DELAY = 5;
  protected Button skinFolderButton = null;
  protected Button skinReloadButton = null;
  protected Checkbox disableLayersCheckbox = null;
  protected EditBox skinSearchField = null;
  private String searchFilter = null;

  public CustomSkinConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void renderSkins(GuiGraphics guiGraphics) {
    if (this.getEasyNPC() == null) {
      return;
    }

    int positionTop = 144;
    int skinPosition = 0;
    skinButtons = new ArrayList<>();

    SkinDataCapable<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    SkinModel skinModel = skinData.getSkinModel();
    Set<UUID> textures = CustomTextureManager.getCustomTextureCacheKeys(skinModel, searchFilter);
    this.numOfSkins = textures.size();

    Object[] textureKeys = textures.toArray();

    // Check Skin buttons state, if number of skins changed.
    if (this.lastNumOfSkins != this.numOfSkins) {
      checkSkinNavigationButtonState();
      this.lastNumOfSkins = this.numOfSkins;
    }

    for (int i = skinStartIndex; i < this.numOfSkins && i < skinStartIndex + maxSkinsPerPage; i++) {
      int left = this.leftPos + 32 + (skinPosition * SKIN_PREVIEW_WIDTH);
      int top = this.topPos + 65 + positionTop;

      // Render Skins
      UUID textureKey = (UUID) textureKeys[i];
      this.renderSkinEntity(guiGraphics, left, top, textureKey);

      // Render skin name
      int topNamePos = Math.round((top - 76f) / SKIN_NAME_SCALING);
      int leftNamePos = Math.round((left - 21f) / SKIN_NAME_SCALING);
      guiGraphics.pose().pushPose();
      guiGraphics.pose().translate(0, 0, 100);
      guiGraphics.pose().scale(SKIN_NAME_SCALING, SKIN_NAME_SCALING, SKIN_NAME_SCALING);
      String variantName = TextUtils.normalizeString(textureKey.toString(), 11);
      Text.drawString(
          guiGraphics,
          this.font,
          variantName,
          leftNamePos,
          topNamePos,
          Constants.FONT_COLOR_DARK_GREEN);
      guiGraphics.pose().popPose();

      skinPosition++;
    }
  }

  private void renderSkinEntity(GuiGraphics guiGraphics, int x, int y, UUID textureUUID) {
    Button skinButton =
        new SkinSelectionButton(
            x - 24,
            y - 81,
            button ->
                NetworkMessageHandlerManager.getServerHandler()
                    .setSkin(
                        this.getEasyNPCUUID(),
                        SkinDataEntry.createCustomSkin(
                            textureUUID,
                            disableLayersCheckbox != null && disableLayersCheckbox.selected())));

    SkinDataCapable<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    UUID skinUUID = skinData.getSkinUUID();
    skinButton.active = !(skinUUID.equals(textureUUID));

    EntityConfigScreenRenderer.renderEntity(
        guiGraphics,
        this.getEasyNPC(),
        EntityRenderConfig.withOverrides(
            x + 4, y - 30, 30, EntityRenderOverrides.withSkin(SkinType.CUSTOM, textureUUID)),
        this.xMouse,
        this.yMouse);

    skinButtons.add(skinButton);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.customSkinButton.active = false;

    // Description text
    setDescriptionText("custom_skin.text");

    // Entity specific information.
    SkinDataCapable<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    SkinModel skinModel = skinData.getSkinModel();
    this.numOfSkins = CustomTextureManager.getCustomTextureCacheKeys(skinModel).size();

    // Skin Navigation Buttons
    defineSkinNavigationButtons();

    // Open Skin Folder Button
    Path skinModelFolder = CustomSkinDataFiles.getCustomSkinDataFolder(skinModel);
    if (skinModelFolder != null) {
      this.skinFolderButton =
          this.addRenderableWidget(
              new TextButton(
                  this.contentLeftPos + 10,
                  this.contentTopPos + 65,
                  263,
                  "open_textures_folder",
                  skinModel.toString(),
                  onPress -> Util.getPlatform().openFile(skinModelFolder.toFile())));
    }

    // Skin Reload Button
    this.skinReloadButton =
        this.addRenderableWidget(
            new ReloadButton(
                this.skinFolderButton.getX() + this.skinFolderButton.getWidth(),
                this.skinFolderButton.getY(),
                17,
                16,
                null,
                onPress -> {
                  CustomSkinDataFiles.refreshRegisterTextureFiles();
                  CustomSkinConfigurationScreen.nextSkinReload =
                      (int) java.time.Instant.now().getEpochSecond() + ADD_SKIN_RELOAD_DELAY;
                }));

    // Skin Search Field
    this.skinSearchField =
        this.addRenderableWidget(
            new SearchField(
                this.font, this.contentLeftPos + 100, this.contentTopPos + 190, 100, 14));
    this.skinSearchField.setResponder(this::onSearchFieldChanged);

    // Disable Layers Checkbox
    this.disableLayersCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 55,
                this.contentTopPos + 85,
                "disable_skin_layers",
                skinData.getSkinDataEntry().disableLayers(),
                checkbox ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .setSkin(
                            this.getEasyNPCUUID(),
                            skinData.getSkinDataEntry().withDisableLayers(checkbox.selected()))));
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Description text
    renderDescriptionText(guiGraphics, this.contentLeftPos + 5, this.contentTopPos + 5);

    // Throttle the skin reload button.
    if (this.skinReloadButton != null) {
      boolean canSkinReload =
          java.time.Instant.now().getEpochSecond() >= CustomSkinConfigurationScreen.nextSkinReload;
      if (!canSkinReload) {
        Text.drawConfigString(
            guiGraphics,
            this.font,
            "skin_reloading",
            this.contentLeftPos + 55,
            this.contentTopPos + 143,
            Constants.FONT_COLOR_RED);
      }
      this.skinReloadButton.active = canSkinReload;
    }

    // Skins
    this.renderSkins(guiGraphics);
  }

  private void onSearchFieldChanged(String searchText) {
    if (searchText != null && !searchText.isEmpty()) {
      this.searchFilter = searchText;
      this.skinStartIndex = 0;
      this.checkSkinNavigationButtonState();
    } else {
      this.searchFilter = "";
    }
  }
}
