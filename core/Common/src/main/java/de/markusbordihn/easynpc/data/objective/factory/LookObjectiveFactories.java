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

package de.markusbordihn.easynpc.data.objective.factory;

import static de.markusbordihn.easynpc.data.objective.factory.BuiltInObjectiveFactories.register;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.LookAtEntityByUUIDGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.LookAtItemGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.ResetLookAtPlayerGoal;
import java.util.UUID;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.RandomLookAroundGoal;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.crafting.Ingredient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

final class LookObjectiveFactories {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private LookObjectiveFactories() {}

  static void registerFactories() {
    register(
        ObjectiveType.LOOK_AT_RESET,
        (objectiveDataEntry, easyNPC) -> new ResetLookAtPlayerGoal<>(easyNPC));

    register(
        ObjectiveType.LOOK_RANDOM_AROUND,
        (objectiveDataEntry, easyNPC) -> new RandomLookAroundGoal(easyNPC.getMob()));

    register(ObjectiveType.LOOK_AT_PLAYER, new LookAtEntityGoalFactory(Player.class));
    register(ObjectiveType.LOOK_AT_MOB, new LookAtEntityGoalFactory(Mob.class));
    register(ObjectiveType.LOOK_AT_ANIMAL, new LookAtEntityGoalFactory(Animal.class));

    register(
        ObjectiveType.LOOK_AT_ENTITY_BY_UUID,
        (objectiveDataEntry, easyNPC) -> {
          UUID lookAtEntityUUID = objectiveDataEntry.getTargetEntityUUID();
          if (lookAtEntityUUID == null) {
            log.debug("No valid target entity UUID for LOOK_AT_ENTITY_BY_UUID objective!");
            return null;
          }

          return new LookAtEntityByUUIDGoal<>(
              easyNPC, lookAtEntityUUID, objectiveDataEntry.getLookDistance());
        });

    register(
        ObjectiveType.LOOK_AT_OWNER,
        (objectiveDataEntry, easyNPC) -> {
          Entity lookAtOwner = objectiveDataEntry.getTargetOwner(easyNPC);
          if (!(lookAtOwner instanceof LivingEntity ownerEntity) || ownerEntity.isRemoved()) {
            log.debug("No valid owner for LOOK_AT_OWNER objective!");
            return null;
          }

          return new LookAtEntityByUUIDGoal<>(
              easyNPC, ownerEntity.getUUID(), objectiveDataEntry.getLookDistance());
        });

    register(
        ObjectiveType.LOOK_AT_ITEM,
        (objectiveDataEntry, easyNPC) -> {
          Ingredient lookAtItems = TargetItemResolver.resolveTargetItems(objectiveDataEntry);
          if (lookAtItems == null) {
            return null;
          }

          return new LookAtItemGoal<>(easyNPC, lookAtItems, objectiveDataEntry.getLookDistance());
        });
  }
}
