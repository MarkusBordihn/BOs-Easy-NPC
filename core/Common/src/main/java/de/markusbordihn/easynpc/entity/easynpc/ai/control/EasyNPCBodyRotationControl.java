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

package de.markusbordihn.easynpc.entity.easynpc.ai.control;

import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.control.BodyRotationControl;

public class EasyNPCBodyRotationControl extends BodyRotationControl {

  private final Mob mob;

  public EasyNPCBodyRotationControl(Mob mob) {
    super(mob);
    this.mob = mob;
  }

  @Override
  public void clientTick() {
    if (this.mob instanceof EasyNPC<?> easyNPC) {
      ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
      if (modelData != null) {
        CustomRotation rootRotation = modelData.getModelRootData().rotation();
        if (rootRotation.locked() && this.mob.walkAnimation.speed() <= 0.01f) {
          this.mob.yBodyRot = rootRotation.y();
          return;
        }
      }
    }

    super.clientTick();
  }
}
