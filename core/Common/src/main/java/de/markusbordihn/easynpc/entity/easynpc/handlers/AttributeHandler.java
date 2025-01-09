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

package de.markusbordihn.easynpc.entity.easynpc.handlers;

import de.markusbordihn.easynpc.data.attribute.CombatAttributes;
import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;

public interface AttributeHandler<E extends PathfinderMob> extends EasyNPC<E> {

  default void checkAttributeActions() {
    this.getProfiler().push("npcCheckAttributeActions");

    // Validate attribute data and mob entity.
    Mob mob = this.getMob();
    AttributeData<?> attributeData = this.getEasyNPCAttributeData();
    if (attributeData == null || mob == null || mob.isDeadOrDying()) {
      return;
    }

    // Check entity attributes.
    EntityAttributes entityAttributes = attributeData.getEntityAttributes();

    // Handle combat relevant attributes.
    CombatAttributes combatAttributes = entityAttributes.getCombatAttributes();
    if (combatAttributes.healthRegeneration() > 0 && mob.getHealth() < mob.getMaxHealth()) {
      mob.setHealth(
          (float)
              Math.min(
                  mob.getMaxHealth(), mob.getHealth() + combatAttributes.healthRegeneration()));
    }

    this.getProfiler().pop();
  }
}
