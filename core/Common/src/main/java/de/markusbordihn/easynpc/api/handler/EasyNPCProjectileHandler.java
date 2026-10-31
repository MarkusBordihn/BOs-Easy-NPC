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

package de.markusbordihn.easynpc.api.handler;

import de.markusbordihn.easynpc.data.attack.ProjectileData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import de.markusbordihn.easynpc.handler.ProjectileHandler;
import java.util.List;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.projectile.Projectile;

/**
 * Projectile settings only take effect for gun attacks while {@link ProjectileData#enabled()} is
 * set. The shoot and create methods work for any living entity, not only for Easy NPCs.
 */
public class EasyNPCProjectileHandler {

  private EasyNPCProjectileHandler() {}

  public static ProjectileData getProjectileData(EasyNPC<?> easyNPC) {
    return ProjectileHandler.getProjectileData(easyNPC);
  }

  public static boolean setProjectileData(EasyNPC<?> easyNPC, ProjectileData projectileData) {
    return ProjectileHandler.setProjectileData(easyNPC, projectileData);
  }

  public static List<Projectile> shootProjectiles(
      LivingEntity shooter, LivingEntity target, ProjectileData projectileData) {
    return AttackHandler.performCustomProjectileAttack(shooter, target, projectileData);
  }

  public static Projectile createProjectile(LivingEntity shooter, ProjectileData projectileData) {
    return AttackHandler.createCustomProjectile(shooter, projectileData);
  }
}
