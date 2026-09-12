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

package de.markusbordihn.easynpc.configui.client.screen.preset;

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.CustomScreen;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.ReloadButton;
import de.markusbordihn.easynpc.configui.client.screen.components.SearchField;
import de.markusbordihn.easynpc.configui.data.preset.PresetFilterType;
import de.markusbordihn.easynpc.configui.data.preset.PresetSortType;
import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.configui.menu.preset.PresetBrowserMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.io.ClientDefaultPresetDataFiles;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.io.LocalPresetDataFiles;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import de.markusbordihn.easynpc.security.NpcSecurityRole;
import de.markusbordihn.easynpc.security.PresetAuthority;
import de.markusbordihn.easynpc.security.PresetFeaturePreview;
import de.markusbordihn.easynpc.security.PresetTrustLevel;
import de.markusbordihn.easynpc.security.SecurityManager;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import de.markusbordihn.easynpc.utils.UUIDUtils;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Stream;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.player.Inventory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetBrowserScreen extends CustomScreen<PresetBrowserMenu, AdditionalScreenData> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final int PREVIEW_Y = 84;
  private static final int LIST_X = 5;
  private static final int LIST_WIDTH = 150;
  private static final int FILTER_Y = 24;
  private static final int SORT_Y = 43;
  private static final int COUNT_Y = 62;
  private static final int LIST_Y = 72;
  private static final int LIST_BOTTOM_MARGIN = 10;
  private static final int CHECKBOX_HEIGHT = 16;
  private static final int INFO_BOX_MIN_HEIGHT = 30;
  private static final int INFO_BOX_MAX_HEIGHT = 70;
  private static final int SPAWN_COOLDOWN_TICKS = 10;
  private static final String[] NO_SEARCH_TERMS = new String[0];

  private final List<PresetListEntry> presetEntries = new ArrayList<>();
  private PresetList presetListWidget;
  private PresetListEntry selectedEntry;
  private Button spawnAsNewButton;
  private Button spawnWithOriginalButton;
  private TextButton sortDirectionButton;
  private Checkbox autoCloseCheckbox;
  private SearchField searchBox;
  private PresetFilterType currentFilter = PresetFilterType.ALL;
  private String[] searchTerms = NO_SEARCH_TERMS;
  private int infoBoxHeight;
  private long lastSpawnTick;

  public PresetBrowserScreen(PresetBrowserMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component, 328, 243);
    this.showCloseButton = true;
    this.renderBackground = false;
    this.renderDefaultScreenBackground = false;
  }

  private static String displayName(PresetListEntry entry) {
    return LocalPresetDataFiles.getPresetDisplayName(entry.getPreset(), entry.getMetadata())
        .toLowerCase(Locale.ROOT);
  }

  private static Component sortDirectionLabel() {
    return TextComponent.getText(PresetBrowserState.isSortDescending() ? "↓" : "↑");
  }

  private static Component sortDirectionTooltip() {
    if (PresetBrowserState.isSortDescending()) {
      return TextComponent.getTranslatedConfigText("preset_browser.sort_descending");
    }

    return TextComponent.getTranslatedConfigText("preset_browser.sort_ascending");
  }

  @Override
  public boolean isPauseScreen() {
    return false;
  }

  @Override
  protected void init() {
    super.init();

    SpinButton<PresetFilterType> filterButton =
        new SpinButton<>(
            LIST_X,
            FILTER_Y,
            68,
            16,
            PresetFilterType.getAllFilters(),
            PresetFilterType.ALL,
            button -> {
              this.currentFilter = button.get();
              this.applyFilters();
            });
    filterButton.setLabelProvider(
        filterType -> TextComponent.getTranslatedConfigText(filterType.getTranslationKey()));
    this.addRenderableWidget(filterButton);

    int searchBoxX = LIST_X + 73;
    this.searchBox =
        new SearchField(this.font, searchBoxX, FILTER_Y, LIST_X + LIST_WIDTH - searchBoxX - 12, 16);
    this.searchBox.setMaxLength(50);
    this.searchBox.setHint(TextComponent.getTranslatedConfigText("preset_browser.search"));
    this.searchBox.setResponder(
        text -> {
          this.updateSearchTerms(text);
          this.applyFilters();
        });
    this.addRenderableWidget(this.searchBox);

    SpinButton<PresetSortType> sortButton =
        new SpinButton<>(
            LIST_X,
            SORT_Y,
            105,
            16,
            PresetSortType.getAllSortTypes(),
            PresetBrowserState.getSortType(),
            button -> {
              PresetBrowserState.setSortType(button.get());
              this.applyFilters();
            });
    sortButton.setLabelProvider(
        sortType -> TextComponent.getTranslatedConfigText(sortType.getTranslationKey()));
    this.addRenderableWidget(sortButton);

    this.sortDirectionButton =
        this.addRenderableWidget(
            new TextButton(
                LIST_X + 108,
                SORT_Y,
                16,
                sortDirectionLabel(),
                button -> {
                  PresetBrowserState.setSortDescending(!PresetBrowserState.isSortDescending());
                  this.sortDirectionButton.setMessage(sortDirectionLabel());
                  this.sortDirectionButton.setTooltip(Tooltip.create(sortDirectionTooltip()));
                  this.applyFilters();
                }));
    this.sortDirectionButton.setTooltip(Tooltip.create(sortDirectionTooltip()));

    this.addRenderableWidget(
        new ReloadButton(
            LIST_X + LIST_WIDTH - 20, SORT_Y, 20, 16, "", button -> this.reloadPresets()));

    this.presetListWidget =
        new PresetList(
            this.minecraft, LIST_WIDTH, this.height - LIST_Y - LIST_BOTTOM_MARGIN, LIST_Y, 30);
    this.presetListWidget.setX(LIST_X);
    this.addWidget(this.presetListWidget);

    int rightPanelX = LIST_X + LIST_WIDTH + 15;
    int buttonWidth = (this.width - rightPanelX - 15) / 2;
    int checkboxY = this.height - LIST_BOTTOM_MARGIN - CHECKBOX_HEIGHT;
    int buttonY = checkboxY - TextButton.DEFAULT_HEIGHT - 6;
    this.infoBoxHeight =
        Math.max(
            INFO_BOX_MIN_HEIGHT,
            Math.min(buttonY - (PREVIEW_Y - 45 + 115) - 5, INFO_BOX_MAX_HEIGHT));

    this.spawnAsNewButton =
        this.addRenderableWidget(
            new TextButton(
                rightPanelX,
                buttonY,
                buttonWidth,
                "preset_browser.spawn_new",
                button -> this.spawnPreset(false)));
    this.spawnAsNewButton.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText("preset_browser.spawn_new_tooltip")));

    this.spawnWithOriginalButton =
        this.addRenderableWidget(
            new TextButton(
                rightPanelX + buttonWidth + 5,
                buttonY,
                buttonWidth,
                "preset_browser.spawn_original",
                button -> this.spawnPreset(true)));

    this.autoCloseCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                rightPanelX,
                checkboxY,
                TextComponent.getTranslatedConfigText("preset_browser.auto_close"),
                PresetBrowserState.isAutoCloseEnabled(),
                true,
                checkbox -> PresetBrowserState.setAutoCloseEnabled(checkbox.selected())));

    this.selectEntry(null);
    this.loadPresetEntries();
    this.applyFilters();
  }

  public boolean isSelected(Identifier preset) {
    return this.selectedEntry != null && this.selectedEntry.getPreset().equals(preset);
  }

  public PresetList getPresetListWidget() {
    return presetListWidget;
  }

  public void selectEntry(PresetListEntry entry) {
    this.selectedEntry = entry;
    this.spawnAsNewButton.active = entry != null;
    this.spawnAsNewButton.visible = entry != null;
    this.spawnWithOriginalButton.visible = entry != null;
    this.updateRestoreButton(entry);
  }

  private void updateRestoreButton(PresetListEntry entry) {
    if (entry == null) {
      this.spawnWithOriginalButton.active = false;
      this.spawnWithOriginalButton.setTooltip(null);
      return;
    }

    if (!entry.hasStoredIdentity()) {
      if (entry.getPresetData() == null) {
        this.blockRestoreButton("preset_browser.restore_unavailable_no_data");
      } else {
        this.blockRestoreButton("preset_browser.restore_unavailable_no_identity");
      }
      return;
    }

    if (!this.canRestoreIdentity()) {
      this.blockRestoreButton("preset_browser.restore_unavailable_no_permission");
      return;
    }

    this.spawnWithOriginalButton.active = true;
    this.spawnWithOriginalButton.setTooltip(
        Tooltip.create(
            TextComponent.getTranslatedConfigText(
                "preset_browser.restore_tooltip", UUIDUtils.shortId(entry.getStoredEntityUUID()))));
  }

  private void blockRestoreButton(String reasonKey) {
    this.spawnWithOriginalButton.active = false;
    this.spawnWithOriginalButton.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText(reasonKey)));
  }

  public boolean canRestoreIdentity() {
    if (this.getAdditionalScreenData() == null
        || this.getAdditionalScreenData().getData() == null) {
      return true;
    }

    return this.getAdditionalScreenData().getData().getBooleanOr("CanRestoreIdentity", true);
  }

  public PresetFeaturePreview createSecurityPreview(PresetData presetData) {
    PresetAuthority presetAuthority = this.createPreviewAuthority();
    if (presetData == null || presetData.data() == null || presetAuthority == null) {
      return null;
    }

    return SecurityManager.previewPresetImport(presetData.data(), presetAuthority);
  }

  private PresetAuthority createPreviewAuthority() {
    if (this.getAdditionalScreenData() == null
        || this.getAdditionalScreenData().getData() == null) {
      return null;
    }

    CompoundTag data = this.getAdditionalScreenData().getData();
    if (!data.contains("SecurityRole") || !data.contains("SecurityCommandLevel")) {
      return null;
    }

    NpcSecurityRole role =
        NpcSecurityRole.parse(
            data.getString("SecurityRole").orElse(""), NpcSecurityRole.NORMAL_PLAYER);
    CommandPermissionLevel commandPermissionLevel =
        CommandPermissionLevel.parse(
            data.getString("SecurityCommandLevel").orElse(""), CommandPermissionLevel.ALL);
    return new PresetAuthority(
        null, commandPermissionLevel, PresetTrustLevel.UNTRUSTED_PLAYER, role);
  }

  private void updateSearchTerms(String text) {
    String normalizedSearch = text.toLowerCase(Locale.ROOT).trim();
    if (normalizedSearch.isEmpty()) {
      this.searchTerms = NO_SEARCH_TERMS;
      return;
    }

    this.searchTerms = normalizedSearch.split("\\s+");
  }

  private boolean matchesFilters(PresetListEntry entry) {
    if (!this.currentFilter.matches(entry.getPresetType())) {
      return false;
    }

    if (this.searchTerms.length == 0) {
      return true;
    }

    PresetMetadata metadata = entry.getMetadata();
    String searchableText =
        (LocalPresetDataFiles.getPresetDisplayName(entry.getPreset(), metadata)
                + ' '
                + entry.getPreset()
                + ' '
                + metadata.description()
                + ' '
                + metadata.category()
                + ' '
                + metadata.author())
            .toLowerCase(Locale.ROOT);

    for (String searchTerm : this.searchTerms) {
      if (!searchableText.contains(searchTerm)) {
        return false;
      }
    }

    return true;
  }

  private Comparator<PresetListEntry> presetComparator() {
    Comparator<PresetListEntry> byName = Comparator.comparing(PresetBrowserScreen::displayName);
    Comparator<PresetListEntry> comparator =
        switch (PresetBrowserState.getSortType()) {
          case NAME -> byName;
          case TYPE ->
              Comparator.comparing((PresetListEntry entry) -> entry.getPresetType().name())
                  .thenComparing(byName);
          case ENTITY_TYPE ->
              Comparator.comparing(
                      (PresetListEntry entry) -> entry.getMetadata().entityTypeId(),
                      Comparator.nullsLast(Comparator.naturalOrder()))
                  .thenComparing(byName);
          case DATE ->
              Comparator.comparingLong((PresetListEntry entry) -> entry.getMetadata().created())
                  .thenComparing(byName);
        };

    if (PresetBrowserState.isSortDescending()) {
      return comparator.reversed();
    }

    return comparator;
  }

  private void applyFilters() {
    this.presetListWidget.replaceEntries(
        this.presetEntries.stream()
            .filter(this::matchesFilters)
            .sorted(this.presetComparator())
            .toList());

    if (this.selectedEntry != null
        && !this.presetListWidget.children().contains(this.selectedEntry)) {
      this.selectEntry(null);
    }
  }

  private void reloadPresets() {
    CustomPresetDataFiles.refreshPresetIdentifiers();
    this.selectEntry(null);
    this.loadPresetEntries();
    this.applyFilters();
  }

  private void loadPresetEntries() {
    this.presetEntries.forEach(PresetListEntry::cleanup);
    this.presetEntries.clear();

    collectPresets(
        PresetType.LOCAL,
        LocalPresetDataFiles.getPresetIdentifiers(),
        LocalPresetDataFiles::getPresetMetadata);

    collectPresets(
        PresetType.DEFAULT,
        ClientDefaultPresetDataFiles.getDefaultPresetIdentifiers(),
        ClientDefaultPresetDataFiles::getPresetMetadata);

    if (this.getAdditionalScreenData() == null) {
      return;
    }

    collectPresets(
        PresetType.CUSTOM,
        loadPresetListFromAdditionalData("CustomPresets").stream(),
        preset -> loadMetadataFromAdditionalData("CustomPresetsMetadata", preset));

    collectPresets(
        PresetType.DATA,
        loadPresetListFromAdditionalData("DataPresets").stream(),
        preset -> loadMetadataFromAdditionalData("DataPresetsMetadata", preset));

    collectPresets(
        PresetType.WORLD,
        loadPresetListFromAdditionalData("WorldPresets").stream(),
        preset -> loadMetadataFromAdditionalData("WorldPresetsMetadata", preset));
  }

  private void collectPresets(
      PresetType presetType,
      Stream<Identifier> presets,
      Function<Identifier, PresetMetadata> metadataProvider) {
    presets.forEach(
        preset ->
            this.presetEntries.add(
                new PresetListEntry(preset, metadataProvider.apply(preset), presetType, this)));
  }

  public CompoundTag getPresetDataFromSync(Identifier preset, PresetType presetType) {
    String dataKey =
        switch (presetType) {
          case CUSTOM -> "CustomPresetsData";
          case WORLD -> "WorldPresetsData";
          default -> null;
        };
    if (dataKey == null || this.getAdditionalScreenData() == null) {
      return null;
    }

    CompoundTag presetsData = this.getAdditionalScreenData().get(dataKey);
    String presetKey = preset.toString();
    if (presetsData.isEmpty() || !presetsData.contains(presetKey)) {
      return null;
    }

    CompoundTag presetTag = presetsData.getCompoundOrEmpty(presetKey);
    return presetTag.isEmpty() ? null : presetTag;
  }

  public CompoundTag getPresetIdentityFromSync(Identifier preset) {
    if (this.getAdditionalScreenData() == null) {
      return null;
    }

    CompoundTag presetsIdentity = this.getAdditionalScreenData().get("PresetsIdentity");
    String presetKey = preset.toString();
    if (presetsIdentity == null || !presetsIdentity.contains(presetKey)) {
      return null;
    }

    CompoundTag identityTag = presetsIdentity.getCompoundOrEmpty(presetKey);
    return identityTag.isEmpty() ? null : identityTag;
  }

  private Set<Identifier> loadPresetListFromAdditionalData(String key) {
    if (this.getAdditionalScreenData() == null) {
      return Collections.emptySet();
    }

    ListTag listTag = this.getAdditionalScreenData().getList(key);
    if (listTag.isEmpty()) {
      return Collections.emptySet();
    }

    return new LinkedHashSet<>(CompoundTagUtils.readIdentifiers(listTag));
  }

  private PresetMetadata loadMetadataFromAdditionalData(String metadataKey, Identifier preset) {
    if (this.getAdditionalScreenData() == null) {
      return PresetMetadata.createDefault();
    }

    CompoundTag metadataMap = this.getAdditionalScreenData().get(metadataKey);
    if (metadataMap == null || !metadataMap.contains(preset.toString())) {
      return PresetMetadata.createDefault();
    }

    return PresetMetadata.fromCompoundTag(
        metadataMap.getCompound(preset.toString()).orElse(new CompoundTag()));
  }

  private void spawnPreset(boolean withOriginal) {
    if (this.selectedEntry == null || this.minecraft == null || this.minecraft.level == null) {
      return;
    }

    long gameTime = this.minecraft.level.getGameTime();
    if (gameTime - this.lastSpawnTick < SPAWN_COOLDOWN_TICKS) {
      return;
    }

    this.lastSpawnTick = gameTime;
    if (withOriginal) {
      this.showConfirmationDialog();
    } else {
      this.executeSpawn(false);
    }
  }

  private void showConfirmationDialog() {
    if (this.minecraft == null || this.selectedEntry == null) {
      return;
    }

    UUID entityUUID = this.selectedEntry.getStoredEntityUUID();
    if (entityUUID == null) {
      return;
    }

    this.minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed) {
                this.executeSpawn(true);
              }
              this.minecraft.setScreen(this);
            },
            TextComponent.getTranslatedConfigText("preset_browser.confirm_title"),
            TextComponent.getTranslatedConfigText(
                "preset_browser.confirm_message", entityUUID.toString()),
            TextComponent.getTranslatedConfigText("preset_browser.confirm_yes"),
            TextComponent.getTranslatedConfigText("preset_browser.confirm_no")));
  }

  private void executeSpawn(boolean withOriginal) {
    if (this.selectedEntry == null) {
      return;
    }

    log.info(
        "Spawning preset {}: {}",
        withOriginal ? "with original UUID/position" : "as new NPC",
        this.selectedEntry.getPreset());

    // For LOCAL presets, send the complete PresetData to the server
    if (this.selectedEntry.getPresetType() == PresetType.LOCAL) {
      if (this.selectedEntry.getPresetData() != null) {
        NetworkMessageHandlerManager.getServerHandler()
            .spawnPresetWithData(this.selectedEntry.getPresetData(), withOriginal);
      } else {
        log.error(
            "Cannot spawn LOCAL preset {}: preset data is null", this.selectedEntry.getPreset());
        return;
      }
    } else {
      NetworkMessageHandlerManager.getServerHandler()
          .spawnPreset(
              this.selectedEntry.getPresetType(), this.selectedEntry.getPreset(), withOriginal);
    }

    if (this.autoCloseCheckbox.selected()) {
      this.onClose();
    }
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
    super.extractRenderState(guiGraphics, mouseX, mouseY, partialTicks);

    Text.drawString(guiGraphics, this.font, this.title, 8, 6);

    if (this.searchBox != null) {
      this.searchBox.extractRenderState(guiGraphics, mouseX, mouseY, partialTicks);
    }

    this.presetListWidget.extractRenderState(guiGraphics, mouseX, mouseY, partialTicks);
    this.renderListStatus(guiGraphics);

    if (this.selectedEntry != null) {
      this.renderPresetTitle(guiGraphics);
      this.renderPreviewPanels(guiGraphics, mouseX, mouseY);
    } else {
      Text.drawString(
          guiGraphics,
          this.font,
          TextComponent.getTranslatedConfigText("preset_browser.select_hint"),
          LIST_X + LIST_WIDTH + 20,
          PREVIEW_Y,
          0x3F3F3F);
    }
  }

  private void renderListStatus(GuiGraphicsExtractor guiGraphics) {
    Text.drawString(
        guiGraphics,
        this.font,
        TextComponent.getTranslatedConfigText(
            "preset_browser.count",
            String.valueOf(this.presetListWidget.children().size()),
            String.valueOf(this.presetEntries.size())),
        LIST_X + 2,
        COUNT_Y,
        0x3F3F3F);

    if (this.presetListWidget.children().isEmpty()) {
      Text.drawString(
          guiGraphics,
          this.font,
          TextComponent.getTranslatedConfigText("preset_browser.empty"),
          LIST_X + 4,
          LIST_Y + 10,
          0x3F3F3F);
    }
  }

  @Override
  protected void renderLabels(GuiGraphicsExtractor guiGraphics, int x, int y) {}

  private void renderPresetTitle(GuiGraphicsExtractor guiGraphics) {
    Text.drawString(
        guiGraphics,
        this.font,
        LocalPresetDataFiles.getPresetDisplayName(
            this.selectedEntry.getPreset(), this.selectedEntry.getMetadata()),
        this.spawnAsNewButton.getX() + 5,
        PREVIEW_Y - 56,
        0x3F3F3F);
  }

  private void renderPreviewPanels(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY) {
    int rightPanelX = this.spawnAsNewButton.getX();
    int rightPanelWidth =
        this.spawnWithOriginalButton.getX() + this.spawnWithOriginalButton.getWidth() - rightPanelX;
    int previewBoxWidth = (rightPanelWidth / 2) - 25;
    int previewBoxY = PREVIEW_Y - 45;

    PresetPreviewView.render(
        guiGraphics,
        this.font,
        this.selectedEntry.getPreviewNPC(),
        rightPanelX,
        previewBoxY,
        previewBoxWidth,
        110,
        PREVIEW_Y,
        mouseX,
        mouseY);

    PresetDetailsView.render(
        guiGraphics,
        this.font,
        this.selectedEntry.getPreviewNPC(),
        this.selectedEntry.getPresetData(),
        this.selectedEntry.getSecurityPreview(),
        rightPanelX + previewBoxWidth + 5,
        previewBoxY,
        rightPanelWidth - previewBoxWidth - 5,
        110);

    PresetInfoView.render(
        guiGraphics,
        this.font,
        this.selectedEntry,
        rightPanelX,
        previewBoxY + 115,
        rightPanelWidth,
        this.infoBoxHeight);
  }

  @Override
  public void removed() {
    this.presetEntries.forEach(PresetListEntry::cleanup);
    super.removed();
  }

  @Override
  public boolean mouseScrolled(double mouseX, double mouseY, double deltaX, double deltaY) {
    return this.presetListWidget.mouseScrolled(mouseX, mouseY, deltaX, deltaY)
        || super.mouseScrolled(mouseX, mouseY, deltaX, deltaY);
  }
}
