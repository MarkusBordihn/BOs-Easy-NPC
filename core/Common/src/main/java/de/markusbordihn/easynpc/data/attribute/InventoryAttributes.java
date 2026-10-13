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
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;

public record InventoryAttributes(boolean dropOnDeath, boolean pickUpItems)
    implements EntityAttributesInterface {

  public static final InventoryAttributes EMPTY = new InventoryAttributes(false, false);

  // Shares the flat EntityAttribute compound with the other attribute groups, so it is a MapCodec
  // and defaults are left out of the tag.
  public static final MapCodec<InventoryAttributes> MAP_CODEC =
      RecordCodecBuilder.mapCodec(
          instance ->
              instance
                  .group(
                      Codec.BOOL
                          .optionalFieldOf(InventoryAttributeType.DROP_ON_DEATH.getTagName(), false)
                          .forGetter(InventoryAttributes::dropOnDeath),
                      Codec.BOOL
                          .optionalFieldOf(InventoryAttributeType.PICK_UP_ITEMS.getTagName(), false)
                          .forGetter(InventoryAttributes::pickUpItems))
                  .apply(instance, InventoryAttributes::new));

  public static InventoryAttributes decode(CompoundTag compoundTag) {
    return MAP_CODEC.codec().parse(NbtOps.INSTANCE, compoundTag).result().orElse(EMPTY);
  }

  public InventoryAttributes withDropOnDeath(boolean dropOnDeath) {
    return new InventoryAttributes(dropOnDeath, this.pickUpItems);
  }

  public InventoryAttributes withPickUpItems(boolean pickUpItems) {
    return new InventoryAttributes(this.dropOnDeath, pickUpItems);
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
