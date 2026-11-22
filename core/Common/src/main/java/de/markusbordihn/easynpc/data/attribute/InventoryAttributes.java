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

package de.markusbordihn.easynpc.data.attribute;

import com.mojang.serialization.Codec;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.markusbordihn.easynpc.entity.easynpc.data.InventoryDataCapable;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;

public record InventoryAttributes(boolean dropOnDeath, boolean pickUpItems, int playerSlotCount)
    implements EntityAttributesInterface {

  public static final int MIN_PLAYER_SLOT_COUNT = 1;
  public static final int MAX_PLAYER_SLOT_COUNT = InventoryDataCapable.INVENTORY_SIZE;
  public static final int DEFAULT_PLAYER_SLOT_COUNT = 9;
  public static final InventoryAttributes EMPTY =
      new InventoryAttributes(false, false, DEFAULT_PLAYER_SLOT_COUNT);

  // Shares the flat EntityAttribute compound with the other attribute groups, so it is a MapCodec
  // and defaults are left out of the tag.
  public static final MapCodec<InventoryAttributes> MAP_CODEC =
      RecordCodecBuilder.mapCodec(instance ->
          instance
              .group(
                  Codec.BOOL
                      .optionalFieldOf(InventoryAttributeType.DROP_ON_DEATH.getTagName(), false)
                      .forGetter(InventoryAttributes::dropOnDeath),
                  Codec.BOOL
                      .optionalFieldOf(InventoryAttributeType.PICK_UP_ITEMS.getTagName(), false)
                      .forGetter(InventoryAttributes::pickUpItems),
                  Codec.INT
                      .optionalFieldOf(
                          InventoryAttributeType.PLAYER_SLOT_COUNT.getTagName(),
                          DEFAULT_PLAYER_SLOT_COUNT)
                      .forGetter(InventoryAttributes::playerSlotCount))
              .apply(instance, InventoryAttributes::new));

  public InventoryAttributes {
    playerSlotCount = clampPlayerSlotCount(playerSlotCount);
  }

  public static int clampPlayerSlotCount(int playerSlotCount) {
    return Math.max(MIN_PLAYER_SLOT_COUNT, Math.min(MAX_PLAYER_SLOT_COUNT, playerSlotCount));
  }

  public static InventoryAttributes decode(CompoundTag compoundTag) {
    return MAP_CODEC.codec().parse(NbtOps.INSTANCE, compoundTag).result().orElse(EMPTY);
  }

  public InventoryAttributes withDropOnDeath(boolean dropOnDeath) {
    return new InventoryAttributes(dropOnDeath, this.pickUpItems, this.playerSlotCount);
  }

  public InventoryAttributes withPickUpItems(boolean pickUpItems) {
    return new InventoryAttributes(this.dropOnDeath, pickUpItems, this.playerSlotCount);
  }

  public InventoryAttributes withPlayerSlotCount(int playerSlotCount) {
    return new InventoryAttributes(this.dropOnDeath, this.pickUpItems, playerSlotCount);
  }

  @Override
  public CompoundTag encode(CompoundTag compoundTag) {
    MAP_CODEC
        .codec()
        .encodeStart(NbtOps.INSTANCE, this)
        .result()
        .filter(CompoundTag.class::isInstance)
        .map(CompoundTag.class::cast)
        .ifPresent(compoundTag::merge);
    return compoundTag;
  }
}
