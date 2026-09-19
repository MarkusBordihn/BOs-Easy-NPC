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

package de.markusbordihn.easynpc.configui.client.screen.configuration.sound;

import de.markusbordihn.easynpc.client.screen.components.RangeSliderButton;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.SelectBox;
import de.markusbordihn.easynpc.configui.client.screen.components.SelectOption;
import de.markusbordihn.easynpc.configui.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.sound.SoundEventManager;
import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.sound.SoundDataEntry;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.client.input.MouseButtonEvent;
import net.minecraft.client.resources.sounds.SimpleSoundInstance;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.Identifier;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.world.entity.player.Inventory;

public class SoundConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  private static final Set<SoundType> VARIANT_SOUND_TYPES =
      EnumSet.of(SoundType.AMBIENT_TAMED, SoundType.AMBIENT_STRAY, SoundType.PET);
  private static final int SOUND_ENTRY_HEIGHT = 36;
  private static final int SOUND_ENTRY_SECOND_LINE_OFFSET = 18;
  private static final int WIDGET_HEIGHT = 16;
  private static final int RESET_BUTTON_WIDTH = 10;
  private static final int PLAY_BUTTON_WIDTH = 13;
  private static final int SELECT_BOX_WIDTH = 160;
  private static final int SLIDER_WIDTH = 56;
  private static final int RESET_COLUMN_OFFSET = 164;
  private static final int VOLUME_COLUMN_OFFSET = 177;
  private static final int PITCH_COLUMN_OFFSET = 237;
  private static final int PLAY_COLUMN_OFFSET = 297;
  private static final float MIN_VOLUME = 0.0f;
  private static final float MAX_VOLUME = 2.0f;
  private static final float MIN_PITCH = 0.5f;
  private static final float MAX_PITCH = 2.0f;
  private static final float SOUND_STEP_SIZE = 0.05f;
  private final Map<SoundType, SoundEntryRow> soundEntryRows = new LinkedHashMap<>();
  protected Button basicSoundButton = null;
  protected Button combatSoundButton = null;
  protected Button interactionSoundButton = null;
  protected Button tradeSoundButton = null;
  protected SoundDataSet soundDataSet = new SoundDataSet();

  public SoundConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private static void playSound(String soundName, float volume, float pitch) {
    Identifier soundLocation = Identifier.tryParse(soundName);
    if (soundLocation == null) {
      return;
    }

    SoundEvent soundEvent = BuiltInRegistries.SOUND_EVENT.getOptional(soundLocation).orElse(null);
    if (soundEvent == null) {
      return;
    }

    Minecraft.getInstance()
        .getSoundManager()
        .play(SimpleSoundInstance.forUI(soundEvent, pitch, volume));
  }

  private static List<SelectOption<String>> getSoundOptions(String soundName) {
    List<SelectOption<String>> soundOptions = SoundEventManager.getSoundEventOptions();
    if (soundName.isEmpty() || SoundEventManager.isKnownSoundEvent(soundName)) {
      return soundOptions;
    }

    List<SelectOption<String>> soundOptionsWithCustomSound = new ArrayList<>(soundOptions);
    soundOptionsWithCustomSound.add(0, SelectOption.of(soundName, soundName));
    return soundOptionsWithCustomSound;
  }

  private static SelectOption<String> createCustomSoundOption(String soundName) {
    Identifier soundLocation = Identifier.tryParse(soundName);
    return soundLocation != null && !SoundEventManager.isKnownSoundEvent(soundName)
        ? SelectOption.of(soundName, soundName)
        : null;
  }

  private static void updateSoundTooltip(SelectBox<String> soundSelectBox) {
    String soundName = soundSelectBox.getSelectedValue();
    if (soundName == null || soundName.isEmpty()) {
      soundSelectBox.setTooltip(null);
      return;
    }

    MutableComponent tooltip = TextComponent.getText(soundName);
    if (!SoundEventManager.isKnownSoundEvent(soundName)) {
      tooltip
          .append(TextComponent.getText("\n"))
          .append(TextComponent.getTranslatedConfigText("sound_select.unknown"));
    }
    soundSelectBox.setTooltip(Tooltip.create(tooltip));
  }

  protected void addSoundEntries(ConfigurationType configurationType, List<SoundType> soundTypes) {
    int soundEntryTop = this.contentTopPos + 16;
    for (SoundType soundType : soundTypes) {
      if (VARIANT_SOUND_TYPES.contains(soundType) && !this.soundDataSet.hasSound(soundType)) {
        continue;
      }

      this.addSoundEntry(configurationType, soundType, soundEntryTop);
      soundEntryTop += SOUND_ENTRY_HEIGHT;
    }
  }

  private void addSoundEntry(ConfigurationType configurationType, SoundType soundType, int top) {
    SoundDataEntry soundDataEntry = this.soundDataSet.getSound(soundType);
    String soundName =
        soundDataEntry != null ? soundDataEntry.getSoundEvent().location().toString() : "";
    Component soundLabel =
        TextComponent.getTranslatedConfigText("sound." + soundType.name().toLowerCase(Locale.ROOT));

    Checkbox enabledCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 6 + this.font.width(soundLabel),
                top,
                TextComponent.getBlankText(),
                soundDataEntry != null && soundDataEntry.isEnabled(),
                false,
                onChange -> this.sendSoundChange(soundType)));
    enabledCheckbox.active = !soundName.isEmpty();
    enabledCheckbox.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText("sound_enabled.tooltip")));

    int secondLineTop = top + SOUND_ENTRY_SECOND_LINE_OFFSET;
    SelectBox<String> soundSelectBox =
        this.addRenderableWidget(
            new SelectBox<>(
                this.contentLeftPos + 2,
                secondLineTop,
                SELECT_BOX_WIDTH,
                WIDGET_HEIGHT,
                getSoundOptions(soundName),
                selectedSound -> this.onSoundSelected(soundType)));
    soundSelectBox.setSearchable(true);
    soundSelectBox.setCustomValueFactory(SoundConfigurationScreen::createCustomSoundOption);
    soundSelectBox.selectByValue(soundName);
    updateSoundTooltip(soundSelectBox);

    Button resetButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos + RESET_COLUMN_OFFSET,
                secondLineTop,
                RESET_BUTTON_WIDTH,
                TextComponent.getText("↺"),
                onPress -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .soundReset(this.getEasyNPCUUID(), soundType);
                  NetworkMessageHandlerManager.getServerHandler()
                      .openConfiguration(this.getEasyNPCUUID(), configurationType);
                }));
    resetButton.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText("sound_reset.tooltip")));

    RangeSliderButton volumeSlider =
        this.addRenderableWidget(
            new RangeSliderButton(
                this.contentLeftPos + VOLUME_COLUMN_OFFSET,
                secondLineTop,
                SLIDER_WIDTH,
                WIDGET_HEIGHT,
                soundDataEntry != null ? soundDataEntry.getVolume() : SoundDataEntry.DEFAULT_VOLUME,
                MIN_VOLUME,
                MAX_VOLUME,
                SoundDataEntry.DEFAULT_VOLUME,
                SOUND_STEP_SIZE,
                SliderButton.Type.DOUBLE,
                false,
                onChange -> this.markSoundEntryDirty(soundType)));
    volumeSlider.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText("sound_volume.tooltip")));

    RangeSliderButton pitchSlider =
        this.addRenderableWidget(
            new RangeSliderButton(
                this.contentLeftPos + PITCH_COLUMN_OFFSET,
                secondLineTop,
                SLIDER_WIDTH,
                WIDGET_HEIGHT,
                soundDataEntry != null ? soundDataEntry.getPitch() : SoundDataEntry.DEFAULT_PITCH,
                MIN_PITCH,
                MAX_PITCH,
                SoundDataEntry.DEFAULT_PITCH,
                SOUND_STEP_SIZE,
                SliderButton.Type.DOUBLE,
                false,
                onChange -> this.markSoundEntryDirty(soundType)));
    pitchSlider.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText("sound_pitch.tooltip")));

    Button playButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos + PLAY_COLUMN_OFFSET,
                secondLineTop,
                PLAY_BUTTON_WIDTH,
                TextComponent.getText("▶"),
                onPress -> this.playSoundEntry(soundType)));
    playButton.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText("sound_play.tooltip")));

    this.soundEntryRows.put(
        soundType,
        new SoundEntryRow(
            soundType, top, soundSelectBox, volumeSlider, pitchSlider, enabledCheckbox));
  }

  private void onSoundSelected(SoundType soundType) {
    SoundEntryRow soundEntryRow = this.soundEntryRows.get(soundType);
    if (soundEntryRow == null) {
      return;
    }

    Checkbox enabledCheckbox = soundEntryRow.enabledCheckbox();
    if (!enabledCheckbox.active) {
      enabledCheckbox.active = true;
      enabledCheckbox.setSelected(true);
    }

    updateSoundTooltip(soundEntryRow.soundSelectBox());
    this.sendSoundChange(soundType);
  }

  private void markSoundEntryDirty(SoundType soundType) {
    SoundEntryRow soundEntryRow = this.soundEntryRows.get(soundType);
    if (soundEntryRow != null) {
      soundEntryRow.setDirty(true);
    }
  }

  private void sendSoundChange(SoundType soundType) {
    SoundEntryRow soundEntryRow = this.soundEntryRows.get(soundType);
    if (soundEntryRow == null) {
      return;
    }

    String soundName = soundEntryRow.soundSelectBox().getSelectedValue();
    if (soundName == null || soundName.isEmpty()) {
      return;
    }

    soundEntryRow.setDirty(false);
    NetworkMessageHandlerManager.getServerHandler()
        .soundChange(
            this.getEasyNPCUUID(),
            soundType,
            soundName,
            soundEntryRow.volumeSlider().getTargetValue(),
            soundEntryRow.pitchSlider().getTargetValue(),
            soundEntryRow.enabledCheckbox().selected());
  }

  private void sendDirtySoundChanges() {
    for (SoundEntryRow soundEntryRow : this.soundEntryRows.values()) {
      if (soundEntryRow.isDirty()) {
        this.sendSoundChange(soundEntryRow.soundType());
      }
    }
  }

  private void playSoundEntry(SoundType soundType) {
    SoundEntryRow soundEntryRow = this.soundEntryRows.get(soundType);
    if (soundEntryRow == null) {
      return;
    }

    String soundName = soundEntryRow.soundSelectBox().getSelectedValue();
    if (soundName != null && !soundName.isEmpty()) {
      playSound(
          soundName,
          soundEntryRow.volumeSlider().getTargetValue(),
          soundEntryRow.pitchSlider().getTargetValue());
    }
  }

  @Override
  public void init() {
    super.init();
    this.deferOverlayRendering = true;

    AdditionalScreenData additionalScreenData = this.getAdditionalScreenData();
    if (additionalScreenData != null) {
      this.soundDataSet = additionalScreenData.getSoundDataSet();
    }
    this.soundEntryRows.clear();

    this.basicSoundButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                50,
                "basic",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.BASIC_SOUND)));
    this.blockButtonWithoutPermission(this.basicSoundButton, ConfigurationType.BASIC_SOUND);

    this.combatSoundButton =
        this.addRenderableWidget(
            new TextButton(
                this.basicSoundButton.getX() + this.basicSoundButton.getWidth(),
                this.buttonTopPos,
                60,
                "combat",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.COMBAT_SOUND)));
    this.blockButtonWithoutPermission(this.combatSoundButton, ConfigurationType.COMBAT_SOUND);

    this.interactionSoundButton =
        this.addRenderableWidget(
            new TextButton(
                this.combatSoundButton.getX() + this.combatSoundButton.getWidth(),
                this.buttonTopPos,
                70,
                "interaction",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(
                            this.getEasyNPCUUID(), ConfigurationType.INTERACTION_SOUND)));
    this.blockButtonWithoutPermission(
        this.interactionSoundButton, ConfigurationType.INTERACTION_SOUND);

    this.tradeSoundButton =
        this.addRenderableWidget(
            new TextButton(
                this.interactionSoundButton.getX() + this.interactionSoundButton.getWidth(),
                this.buttonTopPos,
                50,
                "trade",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.TRADE_SOUND)));
    this.blockButtonWithoutPermission(this.tradeSoundButton, ConfigurationType.TRADE_SOUND);
  }

  @Override
  public boolean mouseReleased(MouseButtonEvent mouseButtonEvent) {
    boolean result = super.mouseReleased(mouseButtonEvent);
    this.sendDirtySoundChanges();
    return result;
  }

  @Override
  public void removed() {
    this.sendDirtySoundChanges();
    super.removed();
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    super.extractRenderState(guiGraphics, x, y, partialTicks);

    if (!this.soundEntryRows.isEmpty()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "sound_volume",
          this.contentLeftPos + VOLUME_COLUMN_OFFSET,
          this.contentTopPos + 3);
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "sound_pitch",
          this.contentLeftPos + PITCH_COLUMN_OFFSET,
          this.contentTopPos + 3);

      for (SoundEntryRow soundEntryRow : this.soundEntryRows.values()) {
        Text.drawConfigString(
            guiGraphics,
            this.font,
            "sound." + soundEntryRow.soundType().name().toLowerCase(Locale.ROOT),
            this.contentLeftPos + 2,
            soundEntryRow.top() + 4);
      }
    }

    this.extractOverlayRenderState(guiGraphics, x, y, partialTicks);
  }

  private static final class SoundEntryRow {

    private final SoundType soundType;
    private final int top;
    private final SelectBox<String> soundSelectBox;
    private final RangeSliderButton volumeSlider;
    private final RangeSliderButton pitchSlider;
    private final Checkbox enabledCheckbox;
    private boolean dirty = false;

    private SoundEntryRow(
        SoundType soundType,
        int top,
        SelectBox<String> soundSelectBox,
        RangeSliderButton volumeSlider,
        RangeSliderButton pitchSlider,
        Checkbox enabledCheckbox) {
      this.soundType = soundType;
      this.top = top;
      this.soundSelectBox = soundSelectBox;
      this.volumeSlider = volumeSlider;
      this.pitchSlider = pitchSlider;
      this.enabledCheckbox = enabledCheckbox;
    }

    private SoundType soundType() {
      return this.soundType;
    }

    private int top() {
      return this.top;
    }

    private SelectBox<String> soundSelectBox() {
      return this.soundSelectBox;
    }

    private RangeSliderButton volumeSlider() {
      return this.volumeSlider;
    }

    private RangeSliderButton pitchSlider() {
      return this.pitchSlider;
    }

    private Checkbox enabledCheckbox() {
      return this.enabledCheckbox;
    }

    private boolean isDirty() {
      return this.dirty;
    }

    private void setDirty(boolean dirty) {
      this.dirty = dirty;
    }
  }
}
