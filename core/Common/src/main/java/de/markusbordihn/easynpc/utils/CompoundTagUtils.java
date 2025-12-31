/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.easynpc.utils;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.IntArrayTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.Style;
import net.minecraft.network.chat.TextColor;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.Entity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CompoundTagUtils {

  public static final String ID_PREFIX = "id_";
  public static final String X_TAG = "X";
  public static final String Y_TAG = "Y";
  public static final String Z_TAG = "Z";
  public static final String UUID_TAG = "UUID";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String CUSTOM_NAME_TAG = "CustomName";
  private static final String TEXT_TAG = "text";
  private static final String COLOR_TAG = "color";

  private CompoundTagUtils() {}

  public static void writeUUID(CompoundTag compoundTag, UUID uuid) {
    writeUUID(compoundTag, UUID_TAG, uuid);
  }

  public static void writeUUID(CompoundTag compoundTag, String key, UUID uuid) {
    if (compoundTag == null || key == null || uuid == null) {
      return;
    }
    long mostSignificantBits = uuid.getMostSignificantBits();
    long leastSignificantBits = uuid.getLeastSignificantBits();
    int[] uuidArray = new int[4];
    uuidArray[0] = (int) (mostSignificantBits >> 32);
    uuidArray[1] = (int) mostSignificantBits;
    uuidArray[2] = (int) (leastSignificantBits >> 32);
    uuidArray[3] = (int) leastSignificantBits;
    compoundTag.put(key, new IntArrayTag(uuidArray));
  }

  public static UUID readUUID(CompoundTag compoundTag) {
    return readUUID(compoundTag, UUID_TAG);
  }

  public static UUID readUUID(CompoundTag compoundTag, String key) {
    if (compoundTag == null || key == null || !compoundTag.contains(key)) {
      return null;
    }
    Tag tag = compoundTag.get(key);
    if (!(tag instanceof IntArrayTag intArrayTag)) {
      return null;
    }
    int[] uuidArray = intArrayTag.getAsIntArray();
    if (uuidArray.length != 4) {
      return null;
    }
    try {
      long mostSignificantBits = ((long) uuidArray[0] << 32) | (uuidArray[1] & 0xFFFFFFFFL);
      long leastSignificantBits = ((long) uuidArray[2] << 32) | (uuidArray[3] & 0xFFFFFFFFL);
      return new UUID(mostSignificantBits, leastSignificantBits);
    } catch (Exception e) {
      return null;
    }
  }

  public static CompoundTag writeBlockPos(BlockPos blockPos) {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putInt(X_TAG, blockPos.getX());
    compoundTag.putInt(Y_TAG, blockPos.getY());
    compoundTag.putInt(Z_TAG, blockPos.getZ());
    return compoundTag;
  }

  public static BlockPos readBlockPos(CompoundTag compoundTag) {
    if (compoundTag == null
        || !compoundTag.contains(X_TAG)
        || !compoundTag.contains(Y_TAG)
        || !compoundTag.contains(Z_TAG)) {
      return BlockPos.ZERO;
    }
    return new BlockPos(
        compoundTag.getInt(X_TAG).orElse(0),
        compoundTag.getInt(Y_TAG).orElse(0),
        compoundTag.getInt(Z_TAG).orElse(0));
  }

  public static CompoundTag writeScale(float x, float y, float z) {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putFloat(X_TAG, x);
    compoundTag.putFloat(Y_TAG, y);
    compoundTag.putFloat(Z_TAG, z);
    return compoundTag;
  }

  public static CompoundTag writeCustomScale(CustomScale customScale) {
    return writeScale(customScale.x(), customScale.y(), customScale.z());
  }

  public static CustomScale readCustomScale(CompoundTag compoundTag) {
    if (compoundTag == null) {
      return null;
    }
    return new CustomScale(
        compoundTag.getFloat(X_TAG).orElse(0.0F),
        compoundTag.getFloat(Y_TAG).orElse(0.0F),
        compoundTag.getFloat(Z_TAG).orElse(0.0F));
  }

  public static Identifier readIdentifier(CompoundTag compoundTag, String name) {
    if (compoundTag == null || !compoundTag.contains(name)) {
      return null;
    }
    String resourceLocationString = compoundTag.getString(name).orElse("");
    if (resourceLocationString.isEmpty()) {
      return null;
    }
    if (!resourceLocationString.contains(":")) {
      return Identifier.fromNamespaceAndPath(Constants.MOD_ID, resourceLocationString);
    }
    String namespace = compoundTag.getString(name).orElse("").split(":")[0];
    String path = compoundTag.getString(name).orElse("").split(":")[1];
    return Identifier.fromNamespaceAndPath(namespace, path);
  }

  public static ListTag writeIdentifiers(Set<Identifier> resourceLocations) {
    ListTag listTag = new ListTag();
    resourceLocations.forEach(
        resourceLocation -> {
          int hashCode = resourceLocation.hashCode();
          CompoundTag compoundTag = new CompoundTag();
          compoundTag.putString(ID_PREFIX + hashCode, resourceLocation.toString());
          listTag.add(compoundTag);
        });
    return listTag;
  }

  public static Set<Identifier> readIdentifiers(ListTag listTag) {
    Set<Identifier> resourceLocations = new HashSet<>();
    listTag.forEach(
        tag -> {
          CompoundTag compoundTag = (CompoundTag) tag;
          compoundTag
              .keySet()
              .forEach(
                  key -> {
                    if (key.startsWith(ID_PREFIX)) {
                      resourceLocations.add(readIdentifier(compoundTag, key));
                    }
                  });
        });
    return resourceLocations;
  }

  public static Component parseLegacyCustomName(String customNameString) {
    if (customNameString == null || !customNameString.startsWith("{")) {
      return null;
    }

    try {
      JsonElement jsonElement = JsonParser.parseString(customNameString);
      if (!jsonElement.isJsonObject()) {
        return null;
      }

      JsonObject jsonObject = jsonElement.getAsJsonObject();
      if (!jsonObject.has(TEXT_TAG)) {
        return null;
      }

      Component component = Component.literal(jsonObject.get(TEXT_TAG).getAsString());
      if (jsonObject.has(COLOR_TAG)) {
        TextColor textColor = parseColor(jsonObject.get(COLOR_TAG).getAsString());
        if (textColor != null) {
          component = component.copy().withStyle(Style.EMPTY.withColor(textColor));
        }
      }

      return component;
    } catch (Exception e) {
      log.warn("Failed to parse legacy CustomName: {}", customNameString, e);
      return null;
    }
  }

  private static TextColor parseColor(String colorString) {
    if (colorString == null || !colorString.startsWith("#")) {
      return null;
    }

    try {
      return TextColor.fromRgb(Integer.parseInt(colorString.substring(1), 16));
    } catch (NumberFormatException e) {
      log.warn("Failed to parse color: {}", colorString);
      return null;
    }
  }

  public static void fixLegacyCustomName(Entity entity, CompoundTag compoundTag) {
    if (!compoundTag.contains(CUSTOM_NAME_TAG)) {
      return;
    }

    try {
      String customNameString = compoundTag.getString(CUSTOM_NAME_TAG).orElse("");
      Component legacyName = parseLegacyCustomName(customNameString);
      if (legacyName != null) {
        entity.setCustomName(legacyName);
        log.debug("Applied legacy CustomName '{}' to entity", customNameString);
      }
    } catch (Exception e) {
      // Ignore if CustomName is in new format
    }
  }
}
