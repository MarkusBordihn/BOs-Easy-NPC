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

import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CloseDoorGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomDoorInteractGoal;
import net.minecraft.world.entity.ai.goal.FloatGoal;

final class AttributeObjectiveFactories {

  private AttributeObjectiveFactories() {}

  static void registerFactories() {
    register(ObjectiveType.FLOAT, (objectiveDataEntry, easyNPC) -> new FloatGoal(easyNPC.getMob()));

    register(
        ObjectiveType.OPEN_DOOR,
        (objectiveDataEntry, easyNPC) -> new CustomDoorInteractGoal(easyNPC.getMob(), false));

    register(
        ObjectiveType.CLOSE_DOOR,
        (objectiveDataEntry, easyNPC) -> new CloseDoorGoal(easyNPC.getMob()));
  }
}
