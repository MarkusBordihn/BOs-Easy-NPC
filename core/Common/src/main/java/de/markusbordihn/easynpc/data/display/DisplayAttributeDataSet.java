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

package de.markusbordihn.easynpc.data.display;

import de.markusbordihn.easynpc.Constants;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record DisplayAttributeDataSet(
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> attributes) {

  public static final StreamCodec<RegistryFriendlyByteBuf, DisplayAttributeDataSet> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public DisplayAttributeDataSet decode(RegistryFriendlyByteBuf registryFriendlyByteBuf) {
          return DisplayAttributeDataSet.decode(registryFriendlyByteBuf);
        }

        @Override
        public void encode(
            RegistryFriendlyByteBuf registryFriendlyByteBuf,
            DisplayAttributeDataSet displayAttributeDataSet) {
          displayAttributeDataSet.encode(registryFriendlyByteBuf);
        }
      };
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public DisplayAttributeDataSet() {
    this(createDefaultAttributes());
  }

  public DisplayAttributeDataSet(ListTag listTag) {
    this(readAttributesFromList(listTag));
  }

  public static DisplayAttributeDataSet createDefault() {
    return new DisplayAttributeDataSet();
  }

  private static EnumMap<DisplayAttributeType, DisplayAttributeEntry> createDefaultAttributes() {
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> map =
        new EnumMap<>(DisplayAttributeType.class);
    map.put(DisplayAttributeType.LIGHT_LEVEL, new DisplayAttributeEntry(7));
    map.put(DisplayAttributeType.VISIBLE, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_AT_DAY, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_AT_NIGHT, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_IN_CREATIVE, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_IN_SPECTATOR, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_IN_STANDARD, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_TO_OWNER, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_TO_TEAM, new DisplayAttributeEntry(true));
    map.put(
        DisplayAttributeType.NAME_VISIBILITY,
        new DisplayAttributeEntry(NameVisibilityType.ALWAYS.toString()));
    return map;
  }

  private static EnumMap<DisplayAttributeType, DisplayAttributeEntry> readAttributesFromList(
      ListTag listTag) {
    if (listTag == null || listTag.isEmpty()) {
      log.debug("Received null or empty ListTag for DisplayAttributeDataSet, using defaults");
      return createDefaultAttributes();
    }

    EnumMap<DisplayAttributeType, DisplayAttributeEntry> map =
        new EnumMap<>(DisplayAttributeType.class);

    for (var entry : listTag) {
      if (!(entry instanceof CompoundTag entryCompoundTag) || !entryCompoundTag.contains("Type")) {
        log.warn("Invalid entry in DisplayAttributeDataSet ListTag: {}", entry);
        continue;
      }

      DisplayAttributeType displayAttributeType =
          DisplayAttributeType.get(entryCompoundTag.getString("Type"));
      if (displayAttributeType != DisplayAttributeType.NONE) {
        map.put(displayAttributeType, new DisplayAttributeEntry(entryCompoundTag));
      }
    }

    return map;
  }

  public static DisplayAttributeDataSet decode(RegistryFriendlyByteBuf buffer) {
    int size = buffer.readVarInt();
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> map =
        new EnumMap<>(DisplayAttributeType.class);

    for (int i = 0; i < size; i++) {
      DisplayAttributeType type = buffer.readEnum(DisplayAttributeType.class);
      boolean boolValue = buffer.readBoolean();
      int intValue = buffer.readVarInt();
      String stringValue = buffer.readUtf();

      if (type == DisplayAttributeType.NONE) {
        continue;
      }
      map.put(type, new DisplayAttributeEntry(boolValue, intValue, stringValue));
    }

    return new DisplayAttributeDataSet(map);
  }

  public boolean hasAttribute(DisplayAttributeType attributeType) {
    return attributes.containsKey(attributeType);
  }

  public DisplayAttributeEntry getAttribute(DisplayAttributeType attributeType) {
    return attributes.get(attributeType);
  }

  public DisplayAttributeDataSet withAttribute(
      DisplayAttributeType attributeType, DisplayAttributeEntry entry) {
    if (attributeType == null || entry == null) {
      return this;
    }
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> newAttributes =
        new EnumMap<>(this.attributes);
    newAttributes.put(attributeType, entry);
    return new DisplayAttributeDataSet(newAttributes);
  }

  public ListTag save() {
    ListTag listTag = new ListTag();

    if (attributes == null || attributes.isEmpty()) {
      return listTag;
    }

    for (var entry : attributes.entrySet()) {
      if (entry.getKey() == DisplayAttributeType.NONE) {
        continue;
      }
      CompoundTag entryTag = new CompoundTag();
      entryTag.putString("Type", entry.getKey().name());
      entry.getValue().write(entryTag);
      listTag.add(entryTag);
    }

    return listTag;
  }

  public void encode(RegistryFriendlyByteBuf buffer) {
    buffer.writeVarInt(
        (int) attributes.keySet().stream().filter(key -> key != DisplayAttributeType.NONE).count());
    for (var entry : attributes.entrySet()) {
      if (entry.getKey() == DisplayAttributeType.NONE) {
        continue;
      }
      buffer.writeEnum(entry.getKey());
      buffer.writeBoolean(entry.getValue().booleanValue());
      buffer.writeVarInt(entry.getValue().intValue());
      buffer.writeUtf(entry.getValue().stringValue());
    }
  }
}
