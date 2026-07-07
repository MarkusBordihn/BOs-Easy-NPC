/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.client.screen.configuration.model;

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.configui.client.screen.components.SearchField;
import de.markusbordihn.easynpc.configui.client.screen.components.SkinSelectionButton;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

public class EasyModelEntitiesModelConfigurationScreen<T extends ConfigurationMenu>
    extends ModelConfigurationScreen<T> {

  public static final String ADDITIONAL_DATA_PROFILES_TAG = "EasyModelProfiles";

  private static final float SKIN_NAME_SCALING = 0.7f;
  private static final int MAX_SKINS_PER_PAGE = 5;
  private static final int SKIN_PREVIEW_WIDTH = 60;

  private Button skinNextButton = null;
  private Button skinNextPageButton = null;
  private Button skinPreviousButton = null;
  private Button skinPreviousPageButton = null;

  private int lastNumOfSkins = 0;
  private int numOfProfiles = 0;
  private int skinStartIndex = 0;
  private String searchFilter = null;

  private List<Button> profileButtons = new ArrayList<>();
  private List<ResourceLocation> profileList = List.of();
  private List<FormattedCharSequence> noteTextComponents = Collections.emptyList();

  public EasyModelEntitiesModelConfigurationScreen(
      T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    this.defaultModelButton.active = false;
    this.customModelButton.active = false;

    setDescriptionText("easy_model_entities_model.text");
    this.noteTextComponents =
        this.font.split(
            TextComponent.getTranslatedConfigText("easy_model_entities_model.note"),
            this.imageWidth - 20);

    this.profileList = loadProfileList();
    this.numOfProfiles = this.profileList.size();

    defineSkinNavigationButtons(this.contentTopPos + 189, this.contentLeftPos, this.rightPos - 29);

    EditBox searchField =
        this.addRenderableWidget(
            new SearchField(
                this.font, this.contentLeftPos + 100, this.contentTopPos + 190, 100, 14));
    searchField.setResponder(this::onSearchFieldChanged);
  }

  private List<ResourceLocation> loadProfileList() {
    // Prefer the server-synced profile list, because Easy Model Entities profiles are loaded
    // from data packs on the server side and are unknown to clients on dedicated servers.
    if (this.getAdditionalScreenData() != null) {
      ListTag profilesTag = this.getAdditionalScreenData().getList(ADDITIONAL_DATA_PROFILES_TAG);
      if (profilesTag != null && !profilesTag.isEmpty()) {
        return CompoundTagUtils.readResourceLocations(profilesTag).stream()
            .sorted(Comparator.comparing(ResourceLocation::toString))
            .toList();
      }
    }
    return IntegrationRegistry.getModels(EasyModelEntitiesManager.INTEGRATION_ID);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    renderDescriptionText(guiGraphics, this.contentLeftPos + 5, this.contentTopPos + 20);

    if (!this.noteTextComponents.isEmpty()) {
      int noteTop = this.contentTopPos + 35;
      for (int i = 0; i < this.noteTextComponents.size(); i++) {
        Text.drawString(
            guiGraphics,
            this.font,
            this.noteTextComponents.get(i),
            this.contentLeftPos + 5,
            noteTop + (i * (this.font.lineHeight + 2)));
      }
    }

    if (!profileButtons.isEmpty()) {
      for (Button button : profileButtons) {
        button.render(guiGraphics, x, y, partialTicks);
      }
    }

    renderProfileList(guiGraphics, x, y);
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    if (!profileButtons.isEmpty()) {
      for (Button skinButton : profileButtons) {
        skinButton.mouseClicked(mouseX, mouseY, button);
      }
    }
    return super.mouseClicked(mouseX, mouseY, button);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);
    renderSkinSelectionBackground(guiGraphics);
  }

  private void defineSkinNavigationButtons(
      int skinButtonTop, int skinButtonLeft, int skinButtonRight) {
    this.skinPreviousPageButton =
        this.addRenderableWidget(
            new TextButton(
                skinButtonLeft,
                skinButtonTop,
                20,
                "<<",
                onPress -> {
                  skinStartIndex = Math.max(this.skinStartIndex - MAX_SKINS_PER_PAGE, 0);
                  this.checkSkinNavigationButtonState();
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
                  this.checkSkinNavigationButtonState();
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
                      && this.skinStartIndex + MAX_SKINS_PER_PAGE < this.numOfProfiles) {
                    this.skinStartIndex = this.skinStartIndex + MAX_SKINS_PER_PAGE;
                  } else if (this.numOfProfiles > MAX_SKINS_PER_PAGE) {
                    this.skinStartIndex = this.numOfProfiles - MAX_SKINS_PER_PAGE;
                  } else {
                    this.skinStartIndex = this.numOfProfiles;
                  }
                  this.checkSkinNavigationButtonState();
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
                      && this.skinStartIndex < this.numOfProfiles - MAX_SKINS_PER_PAGE) {
                    skinStartIndex++;
                  }
                  this.checkSkinNavigationButtonState();
                }));
    this.checkSkinNavigationButtonState();
  }

  private void checkSkinNavigationButtonState() {
    if (this.skinPreviousButton != null) {
      this.skinPreviousButton.active = this.skinStartIndex > 0;
    }
    if (this.skinNextButton != null) {
      this.skinNextButton.active = this.skinStartIndex + MAX_SKINS_PER_PAGE < this.numOfProfiles;
    }
    if (this.skinPreviousPageButton != null) {
      this.skinPreviousPageButton.active = this.skinStartIndex - MAX_SKINS_PER_PAGE > 0;
    }
    if (this.skinNextPageButton != null) {
      this.skinNextPageButton.active =
          this.skinStartIndex + 1 + MAX_SKINS_PER_PAGE < this.numOfProfiles;
    }
  }

  private void renderProfileList(GuiGraphics guiGraphics, int mouseX, int mouseY) {
    if (this.getEasyNPC() == null || this.profileList.isEmpty()) {
      return;
    }

    int positionTop = 144;
    int skinPosition = 0;
    profileButtons = new ArrayList<>();

    List<ResourceLocation> filteredProfiles = this.profileList;
    if (this.searchFilter != null && !this.searchFilter.isEmpty()) {
      filteredProfiles =
          this.profileList.stream()
              .filter(
                  profile ->
                      profile
                          .toString()
                          .toLowerCase(Locale.ROOT)
                          .contains(this.searchFilter.toLowerCase(Locale.ROOT)))
              .toList();
    }
    this.numOfProfiles = filteredProfiles.size();

    if (this.lastNumOfSkins != this.numOfProfiles) {
      checkSkinNavigationButtonState();
      this.lastNumOfSkins = this.numOfProfiles;
    }

    RenderDataCapable<?> renderData = this.getEasyNPC().getEasyNPCRenderData();
    RenderDataEntry currentEntry = renderData.getRenderDataEntry();
    String currentModel = currentEntry.getRenderEntityModel();

    for (int index = skinStartIndex;
        index < this.numOfProfiles && index < skinStartIndex + MAX_SKINS_PER_PAGE;
        index++) {
      ResourceLocation profileId = filteredProfiles.get(index);
      int left = this.leftPos + 32 + (skinPosition * SKIN_PREVIEW_WIDTH);
      int top = this.topPos + 65 + positionTop;

      renderProfileEntity(
          guiGraphics, left, top, profileId, currentModel, renderData, currentEntry);

      int topNamePos = Math.round((top - 76f) / SKIN_NAME_SCALING);
      int leftNamePos = Math.round((left - 21f) / SKIN_NAME_SCALING);
      int scaledMouseX = Math.round(mouseX / SKIN_NAME_SCALING);
      int scaledMouseY = Math.round(mouseY / SKIN_NAME_SCALING);
      guiGraphics.pose().pushPose();
      guiGraphics.pose().translate(0, 0, 100);
      guiGraphics.pose().scale(SKIN_NAME_SCALING, SKIN_NAME_SCALING, SKIN_NAME_SCALING);
      Text.drawLimitedHoverString(
          guiGraphics,
          this.font,
          TextUtils.normalizeString(profileId.getNamespace()),
          leftNamePos,
          topNamePos,
          Constants.FONT_COLOR_DARK_GREEN,
          14,
          scaledMouseX,
          scaledMouseY);
      Text.drawLimitedHoverString(
          guiGraphics,
          this.font,
          TextUtils.normalizeString(profileId.getPath()),
          leftNamePos,
          topNamePos + 10,
          Constants.FONT_COLOR_DARK_GREEN,
          14,
          scaledMouseX,
          scaledMouseY);
      guiGraphics.pose().popPose();

      skinPosition++;
    }
  }

  private void renderProfileEntity(
      GuiGraphics guiGraphics,
      int x,
      int y,
      ResourceLocation profileId,
      String currentModel,
      RenderDataCapable<?> renderData,
      RenderDataEntry originalEntry) {
    String profileKey = profileId.toString();
    Button profileButton =
        new SkinSelectionButton(
            x - 24,
            y - 81,
            button ->
                NetworkMessageHandlerManager.getServerHandler()
                    .setRenderEntityModel(this.getEasyNPCUUID(), profileKey));
    profileButton.active = !profileKey.equals(currentModel);

    renderData.setRenderData(originalEntry.withRenderEntityModel(profileKey));
    IntegrationRegistry.setGuiPreviewMode(true);
    EntityConfigScreenRenderer.renderEntity(
        guiGraphics,
        this.getEasyNPC(),
        EntityRenderConfig.guiScaled(x + 4, y, 30, x - this.xMouse, y - 40 - this.yMouse));
    IntegrationRegistry.setGuiPreviewMode(false);
    renderData.setRenderData(originalEntry);

    profileButtons.add(profileButton);
  }

  private void renderSkinSelectionBackground(GuiGraphics guiGraphics) {
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
