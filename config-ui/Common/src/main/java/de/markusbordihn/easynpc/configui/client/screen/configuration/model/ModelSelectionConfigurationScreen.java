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
import java.util.List;
import java.util.Locale;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

public abstract class ModelSelectionConfigurationScreen<T extends ConfigurationMenu>
    extends ModelConfigurationScreen<T> {

  private static final float NAME_SCALING = 0.7f;
  private static final int ENTRIES_PER_PAGE = 5;
  private static final int PREVIEW_WIDTH = 60;
  private final List<Button> entryButtons = new ArrayList<>();
  private List<ResourceLocation> entries = List.of();
  private List<ResourceLocation> filteredEntries = List.of();
  private List<ResourceLocation> visibleEntries = List.of();
  private List<FormattedCharSequence> noteTextComponents = List.of();
  private String searchFilter = "";
  private int startIndex = 0;
  private Button previousPageButton = null;
  private Button previousButton = null;
  private Button nextButton = null;
  private Button nextPageButton = null;

  protected ModelSelectionConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  protected abstract List<ResourceLocation> loadEntries();

  protected boolean hasEntriesChanged() {
    return false;
  }

  protected boolean isSelectedEntry(ResourceLocation entryId) {
    RenderDataEntry currentEntry = this.getEasyNPC().getEasyNPCRenderData().getRenderDataEntry();
    return entryId.toString().equals(currentEntry.getRenderEntityModel());
  }

  protected void selectEntry(ResourceLocation entryId) {
    NetworkMessageHandlerManager.getServerHandler()
        .setRenderEntityModel(this.getEasyNPCUUID(), entryId.toString());
  }

  protected void renderEntryPreview(
      GuiGraphics guiGraphics, int x, int y, ResourceLocation entryId) {
    RenderDataCapable<?> renderData = this.getEasyNPC().getEasyNPCRenderData();
    RenderDataEntry originalEntry = renderData.getRenderDataEntry();
    renderData.setRenderData(originalEntry.withRenderEntityModel(entryId.toString()));
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
  }

  protected void setNoteText(String textId) {
    this.noteTextComponents =
        this.font.split(TextComponent.getTranslatedConfigText(textId), this.imageWidth - 20);
  }

  @Override
  public void init() {
    super.init();

    int navigationTop = this.contentTopPos + 189;
    int navigationRight = this.rightPos - 29;
    this.previousPageButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos,
                navigationTop,
                20,
                "<<",
                onPress -> this.showEntriesFrom(this.startIndex - ENTRIES_PER_PAGE)));
    this.previousButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos + 20,
                navigationTop,
                20,
                "<",
                onPress -> this.showEntriesFrom(this.startIndex - 1)));
    this.nextPageButton =
        this.addRenderableWidget(
            new TextButton(
                navigationRight,
                navigationTop,
                20,
                ">>",
                onPress -> this.showEntriesFrom(this.startIndex + ENTRIES_PER_PAGE)));
    this.nextButton =
        this.addRenderableWidget(
            new TextButton(
                navigationRight - 20,
                navigationTop,
                20,
                ">",
                onPress -> this.showEntriesFrom(this.startIndex + 1)));

    this.addRenderableWidget(
            new SearchField(
                this.font, this.contentLeftPos + 100, this.contentTopPos + 190, 100, 14))
        .setResponder(this::onSearchFieldChanged);

    this.reloadEntries();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    this.renderDescriptionText(guiGraphics, this.contentLeftPos + 5, this.contentTopPos + 20);
    for (int line = 0; line < this.noteTextComponents.size(); line++) {
      Text.drawString(
          guiGraphics,
          this.font,
          this.noteTextComponents.get(line),
          this.contentLeftPos + 5,
          this.contentTopPos + 35 + line * (this.font.lineHeight + 2));
    }

    if (this.getEasyNPC() == null) {
      return;
    }

    if (this.hasEntriesChanged()) {
      this.reloadEntries();
    }
    for (int position = 0; position < this.visibleEntries.size(); position++) {
      Button entryButton = this.entryButtons.get(position);
      entryButton.active = !this.isSelectedEntry(this.visibleEntries.get(position));
      entryButton.render(guiGraphics, x, y, partialTicks);
    }
    for (int position = 0; position < this.visibleEntries.size(); position++) {
      this.renderEntry(guiGraphics, position, this.visibleEntries.get(position), x, y);
    }
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    for (Button entryButton : this.entryButtons) {
      entryButton.mouseClicked(mouseX, mouseY, button);
    }
    return super.mouseClicked(mouseX, mouseY, button);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);
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

  private void renderEntry(
      GuiGraphics guiGraphics, int position, ResourceLocation entryId, int mouseX, int mouseY) {
    int left = this.getEntryLeft(position);
    int top = this.getEntryTop();
    this.renderEntryPreview(guiGraphics, left, top, entryId);

    int topNamePos = Math.round((top - 76f) / NAME_SCALING);
    int leftNamePos = Math.round((left - 21f) / NAME_SCALING);
    int scaledMouseX = Math.round(mouseX / NAME_SCALING);
    int scaledMouseY = Math.round(mouseY / NAME_SCALING);
    guiGraphics.pose().pushPose();
    guiGraphics.pose().translate(0, 0, 100);
    guiGraphics.pose().scale(NAME_SCALING, NAME_SCALING, NAME_SCALING);
    Text.drawLimitedHoverString(
        guiGraphics,
        this.font,
        TextUtils.normalizeString(entryId.getNamespace()),
        leftNamePos,
        topNamePos,
        Constants.FONT_COLOR_DARK_GREEN,
        14,
        scaledMouseX,
        scaledMouseY);
    Text.drawLimitedHoverString(
        guiGraphics,
        this.font,
        TextUtils.normalizeString(entryId.getPath()),
        leftNamePos,
        topNamePos + 10,
        Constants.FONT_COLOR_DARK_GREEN,
        14,
        scaledMouseX,
        scaledMouseY);
    guiGraphics.pose().popPose();
  }

  private int getEntryLeft(int position) {
    return this.leftPos + 32 + position * PREVIEW_WIDTH;
  }

  private int getEntryTop() {
    return this.topPos + 209;
  }

  private void reloadEntries() {
    this.entries = this.loadEntries();
    this.filterEntries();
  }

  private void filterEntries() {
    this.filteredEntries =
        this.entries.stream()
            .filter(entryId -> entryId.toString().contains(this.searchFilter))
            .toList();
    int numberOfEntries = this.filteredEntries.size();
    this.showEntriesFrom(
        this.startIndex < numberOfEntries
            ? this.startIndex
            : Math.max(0, numberOfEntries - ENTRIES_PER_PAGE));
  }

  private void showEntriesFrom(int startIndex) {
    this.startIndex = startIndex;
    int numberOfEntries = this.filteredEntries.size();
    this.previousButton.active = this.startIndex > 0;
    this.nextButton.active = this.startIndex + ENTRIES_PER_PAGE < numberOfEntries;
    this.previousPageButton.active = this.startIndex - ENTRIES_PER_PAGE > 0;
    this.nextPageButton.active = this.startIndex + 1 + ENTRIES_PER_PAGE < numberOfEntries;

    this.visibleEntries =
        this.filteredEntries.subList(
            this.startIndex, Math.min(numberOfEntries, this.startIndex + ENTRIES_PER_PAGE));
    this.entryButtons.clear();
    for (int position = 0; position < this.visibleEntries.size(); position++) {
      ResourceLocation entryId = this.visibleEntries.get(position);
      this.entryButtons.add(
          new SkinSelectionButton(
              this.getEntryLeft(position) - 24,
              this.getEntryTop() - 81,
              button -> this.selectEntry(entryId)));
    }
  }

  private void onSearchFieldChanged(String searchText) {
    this.searchFilter = searchText.toLowerCase(Locale.ROOT);
    this.startIndex = 0;
    this.filterEntries();
  }
}
