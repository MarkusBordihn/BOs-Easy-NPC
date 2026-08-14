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

package de.markusbordihn.easynpc.data.render;

import com.mojang.serialization.Codec;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.function.UnaryOperator;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;

public record ModelTextureSetting(Map<String, ModelTextureSlot> slots) {

  public static final String DEFAULT_SLOT = "default";
  public static final int MAX_SLOTS = 16;
  public static final int MAX_SLOT_NAME_LENGTH = 64;
  public static final ModelTextureSetting EMPTY = new ModelTextureSetting(Map.of());

  public static final Codec<ModelTextureSetting> CODEC =
      Codec.unboundedMap(Codec.STRING, ModelTextureSlot.CODEC)
          .xmap(ModelTextureSetting::new, ModelTextureSetting::slots);

  private static final char INDEX_PREFIX = '#';

  public ModelTextureSetting {
    slots = normalize(slots);
  }

  public static ModelTextureSetting of(String slot, ResourceLocation texture) {
    return EMPTY.withSlot(slot, texture);
  }

  public static ModelTextureSetting fromTag(Tag tag) {
    if (tag == null) {
      return EMPTY;
    }

    return CODEC.parse(NbtOps.INSTANCE, tag).result().orElse(EMPTY);
  }

  public static Optional<String> normalizeSlot(String slot) {
    if (slot == null) {
      return Optional.empty();
    }

    String normalized = slot.trim().toLowerCase(Locale.ROOT);
    if (normalized.isEmpty()
        || normalized.length() > MAX_SLOT_NAME_LENGTH
        || !isValidSlotName(normalized)) {
      return Optional.empty();
    }
    return Optional.of(normalized);
  }

  private static boolean isValidSlotName(String slot) {
    if (slot.charAt(0) == INDEX_PREFIX) {
      return slot.length() > 1 && isDigits(slot, 1);
    }

    for (int i = 0; i < slot.length(); i++) {
      if (!isSlotNameCharacter(slot.charAt(i))) {
        return false;
      }
    }
    return true;
  }

  private static boolean isSlotNameCharacter(char character) {
    return (character >= 'a' && character <= 'z')
        || (character >= '0' && character <= '9')
        || character == '_'
        || character == '-';
  }

  private static boolean isDigits(String value, int fromIndex) {
    for (int i = fromIndex; i < value.length(); i++) {
      char character = value.charAt(i);
      if (character < '0' || character > '9') {
        return false;
      }
    }
    return true;
  }

  private static Map<String, ModelTextureSlot> normalize(Map<String, ModelTextureSlot> slots) {
    if (slots == null || slots.isEmpty()) {
      return Map.of();
    }

    Map<String, ModelTextureSlot> sorted = new TreeMap<>();
    for (Map.Entry<String, ModelTextureSlot> entry : slots.entrySet()) {
      if (entry.getValue() == null || entry.getValue().isEmpty() || sorted.size() >= MAX_SLOTS) {
        continue;
      }
      normalizeSlot(entry.getKey()).ifPresent(slot -> sorted.put(slot, entry.getValue()));
    }
    return Map.copyOf(sorted);
  }

  public CompoundTag createTag() {
    Tag tag = CODEC.encodeStart(NbtOps.INSTANCE, this).result().orElse(null);
    return tag instanceof CompoundTag compoundTag ? compoundTag : new CompoundTag();
  }

  public boolean isEmpty() {
    return this.slots.isEmpty();
  }

  public Optional<ModelTextureSlot> slot(String slot) {
    return normalizeSlot(slot).map(this.slots::get);
  }

  public Optional<ResourceLocation> texture(String slot) {
    return this.slot(slot).flatMap(ModelTextureSlot::texture);
  }

  public ModelTextureBlend blend(String slot) {
    return this.slot(slot).map(ModelTextureSlot::blend).orElse(ModelTextureBlend.DEFAULT);
  }

  public ModelTextureSetting withSlot(String slot, ResourceLocation texture) {
    if (texture == null) {
      return this;
    }

    return this.withSlot(slot, currentSlot -> currentSlot.withTexture(texture));
  }

  public ModelTextureSetting withSlot(
      String slot, ResourceLocation texture, ModelTextureBlend blend) {
    if (texture == null || blend == null) {
      return this;
    }

    return this.withSlot(slot, currentSlot -> ModelTextureSlot.of(texture, blend));
  }

  public ModelTextureSetting withBlend(String slot, ModelTextureBlend blend) {
    if (blend == null) {
      return this;
    }

    return this.withSlot(slot, currentSlot -> currentSlot.withBlend(blend));
  }

  public ModelTextureSetting withoutSlot(String slot) {
    Optional<String> normalized = normalizeSlot(slot);
    if (normalized.isEmpty() || !this.slots.containsKey(normalized.get())) {
      return this;
    }

    Map<String, ModelTextureSlot> updated = new LinkedHashMap<>(this.slots);
    updated.remove(normalized.get());
    return new ModelTextureSetting(updated);
  }

  public ModelTextureSetting withoutSlots() {
    return EMPTY;
  }

  private ModelTextureSetting withSlot(String slot, UnaryOperator<ModelTextureSlot> update) {
    Optional<String> normalized = normalizeSlot(slot);
    if (normalized.isEmpty()) {
      return this;
    }

    ModelTextureSlot currentSlot =
        this.slots.getOrDefault(normalized.get(), ModelTextureSlot.EMPTY);
    ModelTextureSlot updatedSlot = update.apply(currentSlot);
    if (updatedSlot.equals(currentSlot)) {
      return this;
    }

    Map<String, ModelTextureSlot> updated = new LinkedHashMap<>(this.slots);
    if (updatedSlot.isEmpty()) {
      updated.remove(normalized.get());
    } else {
      updated.put(normalized.get(), updatedSlot);
    }
    return new ModelTextureSetting(updated);
  }
}
