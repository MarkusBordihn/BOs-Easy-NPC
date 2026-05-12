/*
 * Copyright 2025 Markus Bordihn
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
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.input.MouseButtonEvent;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

public class CobblemonModelConfigurationScreen<T extends ConfigurationMenu>
    extends ModelConfigurationScreen<T> {

  private static final float SKIN_NAME_SCALING = 0.7f;
  private static final int MAX_SKINS_PER_PAGE = 5;
  private static final int SKIN_PREVIEW_WIDTH = 60;

  private Button skinNextButton = null;
  private Button skinNextPageButton = null;
  private Button skinPreviousButton = null;
  private Button skinPreviousPageButton = null;

  private int lastNumOfSkins = 0;
  private int numOfSpecies = 0;
  private int skinStartIndex = 0;
  private String searchFilter = null;

  private List<Button> speciesButtons = new ArrayList<>();
  private List<String> speciesList = List.of();
  private List<FormattedCharSequence> noteTextComponents = Collections.emptyList();

  public CobblemonModelConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    this.defaultModelButton.active = false;
    this.customModelButton.active = false;

    setDescriptionText("cobblemon_model.text");
    this.noteTextComponents =
        this.font.split(
            TextComponent.getTranslatedConfigText("cobblemon_model.note"), this.imageWidth - 20);

    this.speciesList = IntegrationRegistry.getModels("cobblemon");
    this.numOfSpecies = this.speciesList.size();

    defineSkinNavigationButtons(this.contentTopPos + 189, this.contentLeftPos, this.rightPos - 29);

    EditBox searchField =
        this.addRenderableWidget(
            new SearchField(
                this.font, this.contentLeftPos + 100, this.contentTopPos + 190, 100, 14));
    searchField.setResponder(this::onSearchFieldChanged);
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

    if (!speciesButtons.isEmpty()) {
      for (Button button : speciesButtons) {
        button.render(guiGraphics, x, y, partialTicks);
      }
    }

    renderSpeciesList(guiGraphics);
  }

  @Override
  public boolean mouseClicked(MouseButtonEvent mouseButtonEvent, boolean doubleClick) {
    if (!speciesButtons.isEmpty()) {
      for (Button skinButton : speciesButtons) {
        skinButton.mouseClicked(mouseButtonEvent, doubleClick);
      }
    }
    return super.mouseClicked(mouseButtonEvent, doubleClick);
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
                      && this.skinStartIndex + MAX_SKINS_PER_PAGE < this.numOfSpecies) {
                    this.skinStartIndex = this.skinStartIndex + MAX_SKINS_PER_PAGE;
                  } else if (this.numOfSpecies > MAX_SKINS_PER_PAGE) {
                    this.skinStartIndex = this.numOfSpecies - MAX_SKINS_PER_PAGE;
                  } else {
                    this.skinStartIndex = this.numOfSpecies;
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
                      && this.skinStartIndex < this.numOfSpecies - MAX_SKINS_PER_PAGE) {
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
      this.skinNextButton.active = this.skinStartIndex + MAX_SKINS_PER_PAGE < this.numOfSpecies;
    }
    if (this.skinPreviousPageButton != null) {
      this.skinPreviousPageButton.active = this.skinStartIndex - MAX_SKINS_PER_PAGE > 0;
    }
    if (this.skinNextPageButton != null) {
      this.skinNextPageButton.active =
          this.skinStartIndex + 1 + MAX_SKINS_PER_PAGE < this.numOfSpecies;
    }
  }

  private void renderSpeciesList(GuiGraphics guiGraphics) {
    if (this.getEasyNPC() == null || this.speciesList.isEmpty()) {
      return;
    }

    int positionTop = 144;
    int skinPosition = 0;
    speciesButtons = new ArrayList<>();

    List<String> filteredSpecies = this.speciesList;
    if (this.searchFilter != null && !this.searchFilter.isEmpty()) {
      filteredSpecies =
          this.speciesList.stream()
              .filter(
                  species ->
                      species
                          .toLowerCase(Locale.ROOT)
                          .contains(this.searchFilter.toLowerCase(Locale.ROOT)))
              .toList();
    }
    this.numOfSpecies = filteredSpecies.size();

    if (this.lastNumOfSkins != this.numOfSpecies) {
      checkSkinNavigationButtonState();
      this.lastNumOfSkins = this.numOfSpecies;
    }

    RenderDataCapable<?> renderData = this.getEasyNPC().getEasyNPCRenderData();
    RenderDataEntry currentEntry = renderData.getRenderDataEntry();
    String currentModel = currentEntry.getRenderEntityModel();

    for (int index = skinStartIndex;
        index < this.numOfSpecies && index < skinStartIndex + MAX_SKINS_PER_PAGE;
        index++) {
      String speciesId = filteredSpecies.get(index);
      int left = this.leftPos + 32 + (skinPosition * SKIN_PREVIEW_WIDTH);
      int top = this.topPos + 65 + positionTop;

      renderSpeciesEntity(
          guiGraphics, left, top, speciesId, currentModel, renderData, currentEntry);

      Identifier speciesLocation = Identifier.tryParse(speciesId);
      String namespace = speciesLocation != null ? speciesLocation.getNamespace() : speciesId;
      String path = speciesLocation != null ? speciesLocation.getPath() : "";
      int topNamePos = Math.round((top - 76f) / SKIN_NAME_SCALING);
      int leftNamePos = Math.round((left - 21f) / SKIN_NAME_SCALING);
      guiGraphics.pose().pushMatrix();
      guiGraphics.pose().scale(SKIN_NAME_SCALING, SKIN_NAME_SCALING);
      Text.drawString(
          guiGraphics,
          this.font,
          TextUtils.normalizeString(namespace, 14),
          leftNamePos,
          topNamePos,
          Constants.FONT_COLOR_DARK_GREEN);
      Text.drawString(
          guiGraphics,
          this.font,
          TextUtils.normalizeString(path, 14),
          leftNamePos,
          topNamePos + 10,
          Constants.FONT_COLOR_DARK_GREEN);
      guiGraphics.pose().popMatrix();

      skinPosition++;
    }
  }

  private void renderSpeciesEntity(
      GuiGraphics guiGraphics,
      int x,
      int y,
      String speciesId,
      String currentModel,
      RenderDataCapable<?> renderData,
      RenderDataEntry originalEntry) {
    String speciesKey = speciesId;
    Button speciesButton =
        new SkinSelectionButton(
            x - 24,
            y - 81,
            button ->
                NetworkMessageHandlerManager.getServerHandler()
                    .setRenderEntityModel(this.getEasyNPCUUID(), speciesKey));
    speciesButton.active = !speciesKey.equals(currentModel);

    renderData.setRenderData(originalEntry.withRenderEntityModel(speciesKey));
    IntegrationRegistry.setGuiPreviewMode(true);
    guiGraphics.enableScissor(x - 24, y - 81, x + 36, y + 3);
    EntityConfigScreenRenderer.renderEntity(
        guiGraphics,
        this.getEasyNPC(),
        EntityRenderConfig.guiScaled(x + 4, y - 30, 28, x - this.xMouse, y - 40 - this.yMouse),
        this.xMouse,
        this.yMouse);
    guiGraphics.disableScissor();
    IntegrationRegistry.setGuiPreviewMode(false);
    renderData.setRenderData(originalEntry);

    speciesButtons.add(speciesButton);
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
