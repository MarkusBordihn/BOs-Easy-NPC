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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import java.util.HashSet;
import java.util.Set;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.ResourceLocation;

public class CompoundTagUtils {

  public static final String ID_PREFIX = "id_";
  public static final String X_TAG = "X";
  public static final String Y_TAG = "Y";
  public static final String Z_TAG = "Z";

  private CompoundTagUtils() {}

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
        compoundTag.getInt(X_TAG), compoundTag.getInt(Y_TAG), compoundTag.getInt(Z_TAG));
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
        compoundTag.getFloat(X_TAG), compoundTag.getFloat(Y_TAG), compoundTag.getFloat(Z_TAG));
  }

  public static ResourceLocation readResourceLocation(CompoundTag compoundTag, String name) {
    if (compoundTag == null || !compoundTag.contains(name)) {
      return null;
    }
    String resourceLocationString = compoundTag.getString(name);
    if (resourceLocationString.isEmpty()) {
      return null;
    }
    if (!resourceLocationString.contains(":")) {
      return ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, resourceLocationString);
    }
    String namespace = compoundTag.getString(name).split(":")[0];
    String path = compoundTag.getString(name).split(":")[1];
    return ResourceLocation.fromNamespaceAndPath(namespace, path);
  }

  public static ListTag writeResourceLocations(Set<ResourceLocation> resourceLocations) {
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

  public static Set<ResourceLocation> readResourceLocations(ListTag listTag) {
    Set<ResourceLocation> resourceLocations = new HashSet<>();
    listTag.forEach(
        tag -> {
          CompoundTag compoundTag = (CompoundTag) tag;
          compoundTag
              .getAllKeys()
              .forEach(
                  key -> {
                    if (key.startsWith(ID_PREFIX)) {
                      resourceLocations.add(readResourceLocation(compoundTag, key));
                    }
                  });
        });
    return resourceLocations;
  }
}
