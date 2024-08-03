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

package de.markusbordihn.easynpc.client.screen.configuration.model;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.client.screen.components.SearchField;
import de.markusbordihn.easynpc.client.screen.components.SkinSelectionButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.data.render.RenderDataSet;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.player.Inventory;

public class CustomModelConfigurationScreen<T extends ConfigurationMenu>
    extends ModelConfigurationScreen<T> {

  private static final float SKIN_NAME_SCALING = 0.7f;
  private static final int MAX_SKINS_PER_PAGE = 5;
  private static final int SKIN_PREVIEW_WIDTH = 60;
  private Button skinNextButton = null;
  private Button skinNextPageButton = null;
  private Button skinPreviousButton = null;
  private Button skinPreviousPageButton = null;
  private EditBox modelSearchField = null;
  private int lastNumOfSkins = 0;
  private int numOfEntities = 0;
  private int skinStartIndex = 0;
  private List<Button> customModelButtons = new ArrayList<>();
  private String searchFilter = null;

  public CustomModelConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.customModelButton.active = false;

    // Description text
    setDescriptionText("custom_model.text");

    // Skin Navigation Buttons
    defineSkinNavigationButtons(this.contentTopPos + 189, this.contentLeftPos, this.rightPos - 29);

    // Model Search Field
    this.modelSearchField =
        this.addRenderableWidget(
            new SearchField(
                this.font, this.contentLeftPos + 100, this.contentTopPos + 190, 100, 14));
    this.modelSearchField.setResponder(this::onSearchFieldChanged);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Description text
    renderDescriptionText(guiGraphics, this.contentLeftPos + 5, this.contentTopPos + 20);

    // Make sure we pass the mouse movements to the dynamically added buttons, if any.
    if (!customModelButtons.isEmpty()) {
      for (Button skinButton : customModelButtons) {
        skinButton.render(guiGraphics, x, y, partialTicks);
      }
    }

    // Render custom models
    this.renderCustomModels(guiGraphics);
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    // Make sure we pass the mouse click to the dynamically added buttons, if any.
    if (!customModelButtons.isEmpty()) {
      for (Button skinButton : customModelButtons) {
        skinButton.mouseClicked(mouseX, mouseY, button);
      }
    }
    return super.mouseClicked(mouseX, mouseY, button);
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
                      && this.skinStartIndex + MAX_SKINS_PER_PAGE < this.numOfEntities) {
                    this.skinStartIndex = this.skinStartIndex + MAX_SKINS_PER_PAGE;
                  } else if (this.numOfEntities > MAX_SKINS_PER_PAGE) {
                    this.skinStartIndex = this.numOfEntities - MAX_SKINS_PER_PAGE;
                  } else {
                    this.skinStartIndex = this.numOfEntities;
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
                      && this.skinStartIndex < this.numOfEntities - MAX_SKINS_PER_PAGE) {
                    skinStartIndex++;
                  }
                  this.checkSkinNavigationButtonState();
                }));
    this.checkSkinNavigationButtonState();
  }

  private void checkSkinNavigationButtonState() {
    // Enable / disable buttons depending on the current skin index.
    if (this.skinPreviousButton != null) {
      this.skinPreviousButton.active = this.skinStartIndex > 0;
    }
    if (this.skinNextButton != null) {
      this.skinNextButton.active = this.skinStartIndex + MAX_SKINS_PER_PAGE < this.numOfEntities;
    }
    if (this.skinPreviousPageButton != null) {
      this.skinPreviousPageButton.active = this.skinStartIndex - MAX_SKINS_PER_PAGE > 0;
    }
    if (this.skinNextPageButton != null) {
      this.skinNextPageButton.active =
          this.skinStartIndex + 1 + MAX_SKINS_PER_PAGE < this.numOfEntities;
    }
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);
    this.renderSkinSelectionBackground(guiGraphics);
  }

  private void renderCustomModels(GuiGraphics guiGraphics) {
    if (this.getEasyNPC() == null) {
      return;
    }

    int positionTop = 144;
    int skinPosition = 0;
    customModelButtons = new ArrayList<>();

    // Get all supported and unknown entity types.
    List<EntityType<? extends Entity>> entityKeys =
        EntityTypeManager.getUnknownAndSupportedEntityTypes();
    if (this.searchFilter != null && !this.searchFilter.isEmpty()) {
      entityKeys.removeIf(
          entityType ->
              !BuiltInRegistries.ENTITY_TYPE
                  .getKey(entityType)
                  .toString()
                  .toLowerCase()
                  .contains(this.searchFilter.toLowerCase()));
    }
    this.numOfEntities = entityKeys.size();

    // Check Skin buttons state, if number of skins changed.
    if (this.lastNumOfSkins != this.numOfEntities) {
      checkSkinNavigationButtonState();
      this.lastNumOfSkins = this.numOfEntities;
    }

    for (int index = skinStartIndex;
        index < this.numOfEntities && index < skinStartIndex + MAX_SKINS_PER_PAGE;
        index++) {
      int left = this.leftPos + 32 + (skinPosition * SKIN_PREVIEW_WIDTH);
      int top = this.topPos + 65 + positionTop;

      // Render Skins
      EntityType<? extends Entity> entityType = entityKeys.get(index);
      this.renderCustomModelEntity(guiGraphics, left, top, entityType);

      // Render skin name
      int topNamePos = Math.round((top - 76f) / SKIN_NAME_SCALING);
      int leftNamePos = Math.round((left - 21f) / SKIN_NAME_SCALING);
      guiGraphics.pose().pushPose();
      guiGraphics.pose().translate(0, 0, 100);
      guiGraphics.pose().scale(SKIN_NAME_SCALING, SKIN_NAME_SCALING, SKIN_NAME_SCALING);
      ResourceLocation entityTypeKey = BuiltInRegistries.ENTITY_TYPE.getKey(entityType);
      Text.drawString(
          guiGraphics,
          this.font,
          TextUtils.normalizeString(entityTypeKey.getNamespace(), 14),
          leftNamePos,
          topNamePos,
          Constants.FONT_COLOR_DARK_GREEN);
      Text.drawString(
          guiGraphics,
          this.font,
          TextUtils.normalizeString(entityTypeKey.getPath(), 14),
          leftNamePos,
          topNamePos + 10,
          Constants.FONT_COLOR_DARK_GREEN);
      guiGraphics.pose().popPose();

      skinPosition++;
    }
  }

  private void renderCustomModelEntity(
      GuiGraphics guiGraphics, int x, int y, EntityType<? extends Entity> entityType) {
    // Create dynamically button for each skin variant.
    Button customModelButton =
        new SkinSelectionButton(
            x - 24,
            y - 81,
            button ->
                NetworkMessageHandlerManager.getServerHandler()
                    .setRenderEntityType(this.getEasyNPCUUID(), entityType));

    // Disable button for active skin.
    RenderData<?> renderData = this.getEasyNPC().getEasyNPCRenderData();
    RenderDataSet renderDataSet = renderData.getRenderDataSet();
    EntityType<?> currentEntityType = renderDataSet.getRenderEntityType();
    customModelButton.active = currentEntityType == null || !(currentEntityType.equals(entityType));

    // Get additional information for the entity type.
    float scaleFactor = EntityTypeManager.getScaleFactor(entityType);

    // Render skin entity with variant and profession.
    ScreenHelper.renderEntityCustomModel(
        guiGraphics,
        x + 4,
        y,
        Math.round(30 / scaleFactor),
        x - this.xMouse,
        y - 40 - this.yMouse,
        this.getEasyNPC(),
        entityType);

    customModelButtons.add(customModelButton);
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
