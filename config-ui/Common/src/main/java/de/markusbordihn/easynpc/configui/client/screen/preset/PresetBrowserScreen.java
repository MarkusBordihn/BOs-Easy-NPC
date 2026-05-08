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
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.CustomScreen;
import de.markusbordihn.easynpc.configui.data.preset.PresetFilterType;
import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.configui.menu.preset.PresetBrowserMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.io.ClientDefaultPresetDataFiles;
import de.markusbordihn.easynpc.io.LocalPresetDataFiles;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import de.markusbordihn.easynpc.security.NpcSecurityRole;
import de.markusbordihn.easynpc.security.PresetAuthority;
import de.markusbordihn.easynpc.security.PresetFeaturePreview;
import de.markusbordihn.easynpc.security.PresetTrustLevel;
import de.markusbordihn.easynpc.security.SecurityManager;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Inventory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetBrowserScreen extends CustomScreen<PresetBrowserMenu, AdditionalScreenData> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final int PREVIEW_Y = 90;
  private PresetList presetListWidget;
  private PresetListEntry selectedEntry;
  private Button spawnAsNewButton;
  private Button spawnWithOriginalButton;
  private TextField searchBox;
  private PresetFilterType currentFilter = PresetFilterType.ALL;
  private String searchFilter = "";
  private int infoBoxHeight;

  public PresetBrowserScreen(PresetBrowserMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component, 328, 243);
    this.showCloseButton = true;
    this.renderBackground = false;
    this.renderDefaultScreenBackground = false;
  }

  @Override
  public boolean isPauseScreen() {
    return false;
  }

  @Override
  protected void init() {
    super.init();

    int listWidth = 150;
    int listX = 5;
    int listY = 55;
    int filterY = 30;

    this.addRenderableWidget(
        new SpinButton<>(
            listX,
            filterY,
            68,
            16,
            PresetFilterType.getFilterNames(),
            PresetFilterType.ALL.name(),
            button -> {
              this.currentFilter = PresetFilterType.valueOf(button.get());
              refreshPresetList();
            }));

    this.searchBox = new TextField(this.font, listX + 73, filterY, listWidth - 65);
    this.searchBox.setMaxLength(50);
    this.searchBox.setHint(Component.translatable("text.easy_npc.config.preset_browser.search"));
    this.searchBox.setResponder(
        text -> {
          this.searchFilter = text.toLowerCase();
          refreshPresetList();
        });
    this.addRenderableWidget(this.searchBox);

    this.presetListWidget = new PresetList(this.minecraft, listWidth, this.height - 65, listY, 30);
    this.addWidget(this.presetListWidget);

    this.infoBoxHeight = Math.min((int) (this.height * 0.22f), 70);

    // Spawn Buttons
    this.spawnAsNewButton =
        this.addRenderableWidget(
            new TextButton(
                listX + listWidth + 15,
                this.height - 35,
                (this.width - (listX + listWidth + 15) - 10 - 5) / 2,
                "preset_browser.spawn_new",
                button -> spawnPreset(false)));
    this.spawnAsNewButton.active = false;
    this.spawnAsNewButton.visible = false;

    this.spawnWithOriginalButton =
        this.addRenderableWidget(
            new TextButton(
                this.spawnAsNewButton.getX() + this.spawnAsNewButton.getWidth() + 5,
                this.spawnAsNewButton.getY(),
                this.spawnAsNewButton.getWidth(),
                "preset_browser.spawn_original",
                button -> spawnPreset(true)));
    this.spawnWithOriginalButton.active = false;
    this.spawnWithOriginalButton.visible = false;
    loadPresets();
  }

  public boolean isSelected(ResourceLocation preset) {
    return selectedEntry != null && selectedEntry.getPreset().equals(preset);
  }

  public void selectEntry(PresetListEntry entry) {
    this.selectedEntry = entry;
    this.spawnAsNewButton.active = entry != null;
    this.spawnAsNewButton.visible = entry != null;

    // Show "Spawn Original" button only if preset has UUID
    boolean hasUUID =
        entry != null
            && entry.getPresetData() != null
            && entry.getPresetData().data() != null
            && entry.getPresetData().data().hasUUID(Entity.UUID_TAG);
    this.spawnWithOriginalButton.active = hasUUID;
    this.spawnWithOriginalButton.visible = hasUUID;
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
        NpcSecurityRole.parse(data.getString("SecurityRole"), NpcSecurityRole.NORMAL_PLAYER);
    CommandPermissionLevel commandPermissionLevel =
        CommandPermissionLevel.parse(
            data.getString("SecurityCommandLevel"), CommandPermissionLevel.ALL);
    return new PresetAuthority(
        null, commandPermissionLevel, PresetTrustLevel.UNTRUSTED_PLAYER, role);
  }

  private boolean matchesFilters(
      ResourceLocation preset, PresetMetadata metadata, PresetType type) {
    if (!currentFilter.matches(type)) {
      return false;
    }

    if (!searchFilter.isEmpty()) {
      return LocalPresetDataFiles.getPresetDisplayName(preset, metadata)
              .toLowerCase()
              .contains(searchFilter)
          || preset.getPath().toLowerCase().contains(searchFilter)
          || metadata.description().toLowerCase().contains(searchFilter)
          || metadata.category().toLowerCase().contains(searchFilter)
          || metadata.author().toLowerCase().contains(searchFilter);
    }

    return true;
  }

  private void refreshPresetList() {
    this.presetListWidget.children().clear();
    this.selectedEntry = null;
    this.spawnAsNewButton.active = false;
    this.spawnWithOriginalButton.active = false;
    this.spawnWithOriginalButton.visible = false;
    loadPresets();
  }

  private void loadPresets() {
    // Always load LOCAL presets from client-side config folder
    loadPresetsOfType(
        PresetType.LOCAL,
        LocalPresetDataFiles.getPresetResourceLocations(),
        LocalPresetDataFiles::getPresetMetadata);

    // Always load DEFAULT presets from client-side JAR
    loadPresetsOfType(
        PresetType.DEFAULT,
        ClientDefaultPresetDataFiles.getDefaultPresetResourceLocations(),
        ClientDefaultPresetDataFiles::getPresetMetadata);

    // Load server-synced preset lists if available
    if (this.getAdditionalScreenData() != null) {
      loadPresetsFromServerSync();
    }
  }

  private void loadPresetsFromServerSync() {
    // Load CUSTOM presets (server config/easy_npc/preset)
    Set<ResourceLocation> customPresets = loadPresetListFromAdditionalData("CustomPresets");
    if (!customPresets.isEmpty()) {
      loadPresetsOfType(
          PresetType.CUSTOM,
          customPresets.stream(),
          preset -> loadMetadataFromAdditionalData("CustomPresetsMetadata", preset));
    }

    // Load DATA presets (datapacks)
    Set<ResourceLocation> dataPresets = loadPresetListFromAdditionalData("DataPresets");
    if (!dataPresets.isEmpty()) {
      loadPresetsOfType(
          PresetType.DATA,
          dataPresets.stream(),
          preset -> loadMetadataFromAdditionalData("DataPresetsMetadata", preset));
    }

    // Load WORLD presets
    Set<ResourceLocation> worldPresets = loadPresetListFromAdditionalData("WorldPresets");
    if (!worldPresets.isEmpty()) {
      loadPresetsOfType(
          PresetType.WORLD,
          worldPresets.stream(),
          preset -> loadMetadataFromAdditionalData("WorldPresetsMetadata", preset));
    }
  }

  public CompoundTag getPresetDataFromSync(ResourceLocation preset, PresetType presetType) {
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

    CompoundTag presetTag = presetsData.getCompound(presetKey);
    return presetTag.isEmpty() ? null : presetTag;
  }

  private Set<ResourceLocation> loadPresetListFromAdditionalData(String key) {
    if (this.getAdditionalScreenData() == null) {
      return Collections.emptySet();
    }

    ListTag listTag = this.getAdditionalScreenData().getList(key);
    if (listTag.isEmpty()) {
      return Collections.emptySet();
    }

    return new HashSet<>(CompoundTagUtils.readResourceLocations(listTag));
  }

  private PresetMetadata loadMetadataFromAdditionalData(
      String metadataKey, ResourceLocation preset) {
    if (this.getAdditionalScreenData() == null) {
      return PresetMetadata.createDefault();
    }

    CompoundTag metadataMap = this.getAdditionalScreenData().get(metadataKey);
    if (metadataMap == null || !metadataMap.contains(preset.toString())) {
      return PresetMetadata.createDefault();
    }

    return PresetMetadata.fromCompoundTag(metadataMap.getCompound(preset.toString()));
  }

  private void loadPresetsOfType(
      PresetType type,
      Stream<ResourceLocation> presets,
      Function<ResourceLocation, PresetMetadata> metadataProvider) {
    if (!currentFilter.matches(type)) {
      return;
    }
    presets.forEach(
        preset -> {
          PresetMetadata metadata = metadataProvider.apply(preset);
          if (matchesFilters(preset, metadata, type)) {
            this.presetListWidget.addEntry(new PresetListEntry(preset, metadata, type, this));
          }
        });
  }

  private void spawnPreset(boolean withOriginal) {
    if (this.selectedEntry == null) {
      return;
    }

    // Show confirmation dialog if spawning with original UUID
    if (withOriginal) {
      showConfirmationDialog();
    } else {
      executeSpawn(false);
    }
  }

  private void showConfirmationDialog() {
    if (this.minecraft == null || this.selectedEntry == null) {
      return;
    }

    // Get UUID from preset data
    String uuid = "Unknown";
    if (this.selectedEntry.getPresetData() != null
        && this.selectedEntry.getPresetData().data() != null
        && this.selectedEntry.getPresetData().data().hasUUID(Entity.UUID_TAG)) {
      uuid = this.selectedEntry.getPresetData().data().getUUID(Entity.UUID_TAG).toString();
    }

    this.minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed) {
                executeSpawn(true);
              }
              this.minecraft.setScreen(this);
            },
            Component.translatable("text.easy_npc.config.preset_browser.confirm_title"),
            Component.translatable("text.easy_npc.config.preset_browser.confirm_message", uuid),
            Component.translatable("text.easy_npc.config.preset_browser.confirm_yes"),
            Component.translatable("text.easy_npc.config.preset_browser.confirm_no")));
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
    if (this.selectedEntry.getPresetType() == PresetType.LOCAL
        && this.selectedEntry.getPresetData() != null) {
      NetworkMessageHandlerManager.getServerHandler()
          .spawnPresetWithData(this.selectedEntry.getPresetData(), withOriginal);
    } else {
      // For other preset types (CUSTOM, DATA, WORLD, DEFAULT), just send the preset reference
      NetworkMessageHandlerManager.getServerHandler()
          .spawnPreset(
              this.selectedEntry.getPresetType(), this.selectedEntry.getPreset(), withOriginal);
    }

    this.onClose();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {
    // Render parent (includes background from CustomScreen)
    super.render(guiGraphics, mouseX, mouseY, partialTicks);

    // Render search box on top of everything
    if (this.searchBox != null) {
      this.searchBox.render(guiGraphics, mouseX, mouseY, partialTicks);
    }

    // Render preset list
    this.presetListWidget.render(guiGraphics, mouseX, mouseY, partialTicks);

    // Render selected preset details
    if (this.selectedEntry != null) {
      this.renderPresetTitle(guiGraphics);
      this.renderPreviewPanels(guiGraphics, mouseX, mouseY);
    }
  }

  private void renderPresetTitle(GuiGraphics guiGraphics) {
    Text.drawString(
        guiGraphics,
        this.font,
        LocalPresetDataFiles.getPresetDisplayName(
            this.selectedEntry.getPreset(), this.selectedEntry.getMetadata()),
        this.spawnAsNewButton.getX() + 5,
        PREVIEW_Y - 55,
        0x3F3F3F);
  }

  private void renderPreviewPanels(GuiGraphics guiGraphics, int mouseX, int mouseY) {
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
        this.selectedEntry.getPreset(),
        this.selectedEntry.getMetadata(),
        rightPanelX,
        previewBoxY + 115,
        rightPanelWidth,
        this.infoBoxHeight);

    this.spawnAsNewButton.setY(previewBoxY + 115 + this.infoBoxHeight + 5);
    this.spawnWithOriginalButton.setY(this.spawnAsNewButton.getY());
  }

  @Override
  public void removed() {
    if (this.presetListWidget != null) {
      this.presetListWidget.removed();
    }
    super.removed();
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    return this.presetListWidget.mouseClicked(mouseX, mouseY, button)
        || super.mouseClicked(mouseX, mouseY, button);
  }

  @Override
  public boolean mouseScrolled(double mouseX, double mouseY, double deltaX, double deltaY) {
    return this.presetListWidget.mouseScrolled(mouseX, mouseY, deltaX, deltaY)
        || super.mouseScrolled(mouseX, mouseY, deltaX, deltaY);
  }
}
