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

package de.markusbordihn.easynpc.entity.easynpc.ai.goal;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Comparator;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.item.crafting.Ingredient;

public class LookAtItemGoal<T extends EasyNPC<?>> extends LookAtTargetGoal<T> {

  private final Ingredient items;

  public LookAtItemGoal(T easyNPC, Ingredient items, float lookDistance) {
    super(easyNPC, lookDistance);
    this.items = items;
  }

  @Override
  protected Entity findTargetEntity() {
    return this.mob
        .level()
        .getEntitiesOfClass(
            ItemEntity.class,
            this.mob.getBoundingBox().inflate(this.lookDistance),
            itemEntity -> itemEntity.isAlive() && this.items.test(itemEntity.getItem()))
        .stream()
        .min(Comparator.comparingDouble(this.mob::distanceToSqr))
        .orElse(null);
  }
}
