package de.markusbordihn.easynpc.utils;

import de.markusbordihn.easynpc.Constants;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.SpawnData;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SpawnerUtils {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static boolean setNextSpawnData(
      BaseSpawner spawner, LevelAccessor level, BlockPos blockPos, SpawnData spawnData) {
    try {
      Method method = findSetNextSpawnDataMethod();
      if (method == null) {
        return false;
      }
      method.invoke(spawner, level, blockPos, spawnData);
      return true;
    } catch (IllegalAccessException | InvocationTargetException e) {
      log.error(
          "Failed to call setNextSpawnData for {} at {} with {}", spawner, blockPos, spawnData, e);
    }
    return false;
  }

  private static Method findSetNextSpawnDataMethod() {
    try {
      for (Method method : BaseSpawner.class.getDeclaredMethods()) {
        Class<?>[] params = method.getParameterTypes();
        if (params.length == 3
            && LevelAccessor.class.isAssignableFrom(params[0])
            && BlockPos.class.isAssignableFrom(params[1])
            && SpawnData.class.isAssignableFrom(params[2])) {

          method.setAccessible(true);
          return method;
        }
      }
    } catch (SecurityException e) {
      log.error("Security exception while accessing BaseSpawner methods", e);
    }
    log.error("Could not find method setNextSpawnData(LevelAccessor, BlockPos, SpawnData)");
    return null;
  }
}
